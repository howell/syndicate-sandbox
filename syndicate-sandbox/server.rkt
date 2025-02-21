#lang racket

(provide run-server
         phx-host
         phx-port)

(require "session.rkt"
         "trace-combiner.rkt"
         "dataspace-trace-integrator.rkt"
         racket/date
         net/url
         json
         web-server/servlet-env
         web-server/http/json
         web-server/dispatch
         web-server/http/request-structs
         net/http-client)

(define-logger sandbox-server)

(struct active-session (session last-activity next-seq-nos) #:transparent)

(define session-envs (make-hash))

(define phx-host (make-parameter (or (getenv "PHX_HOST") "localhost")))
(define phx-port (make-parameter (let ([p (getenv "PHX_PORT")])
                                   (if p
                                       (string->number p)
                                       4000))))


(define (create-session id)
  (define s (new-session #:id id))
  (hash-set! session-envs id (active-session s
                                             (current-inexact-milliseconds)
                                             (hash 'stdout 0
                                                   'stderr 0
                                                   TRACE-TYPE 0)))
  (service-session s))

(define (evaluate-code id code)
  (define s (hash-ref session-envs id #f))
  (if s
      (with-handlers ([exn:fail?
                       (λ (e) (format "Error: ~a" (exn-message e)))])
        (begin
          (mark-activity! s)
          (let ([result (session-eval (active-session-session s) code)])
            (if (void? result)
                ""
                (~v result)))))
      "Error: Session not found"))

(define (mark-activity! s)
  (hash-set! session-envs
             (session-id (active-session-session s))
             (struct-copy active-session s [last-activity (current-inexact-milliseconds)])))

(define (handle-new-session req)
  (define msg (bytes->jsexpr (request-post-data/raw req)))
  (log-sandbox-server-info "~a: Received new session request with body ~a" (timestamp) msg)
  (define id (hash-ref msg 'session_id))
  (with-handlers ([exn?
                   (lambda (e)
                     (response/jsexpr #:code 500
                                      (hash 'reason (exn-message e))))])
    (create-session id)
    (response/jsexpr (hash 'status "ok"))))

(define (handle-submit req)
  (define msg (bytes->jsexpr (request-post-data/raw req)))
  (log-sandbox-server-info "~a: Received code submission request with body ~a" (timestamp) msg)
  (define id (hash-ref msg 'session_id))
  (define code (hash-ref msg 'code))
  (response/jsexpr (hash 'status "ok" 'result (evaluate-code id code))))

(define (handle-keep-alive req)
  (define msg (bytes->jsexpr (request-post-data/raw req)))
  (log-sandbox-server-info "~a: Received keep alive request with body ~a" (timestamp) msg)
  (define id (hash-ref msg 'session_id))
  (define s (hash-ref session-envs id #f))
  (cond
    [s
     (mark-activity! s)
     (response/jsexpr (hash 'status "ok"))]
    [else
     (response/jsexpr #:code 404 "")])
  )

(define (handle-status _req id)
  (log-sandbox-server-info "~a: Received status request for session ~a" (timestamp) id)
  (match (hash-ref session-envs id #f)
    [#f
     (response/jsexpr #:code 404 "")]
    [s
     (mark-activity! s)
     (response/jsexpr #:code 200 "")]))

(define (handle-unknown _req)
  (response/jsexpr (hash 'status "Unknown command")))

(define-values (dispatch-request format-url)
  (dispatch-rules
   [("new")
    #:method "post"
    handle-new-session]
   [("submit")
    #:method "post"
    handle-submit]
   [("status" (string-arg))
    #:method "get"
    handle-status]
   [("keep_alive")
    #:method "post"
    handle-keep-alive]
   [else
    handle-unknown]))

(define (handle-request req)
  (log-sandbox-server-info "~a: Received ~a request to ~a" (timestamp) (request-method req) (url->string (request-uri req)))
  (dispatch-request req))

(define (run-server #:host host
                    #:port port)
  (log-sandbox-server-info "~a: Server running on port ~a" (timestamp) port)
  (serve/servlet handle-request
                 #:port port
                 #:listen-ip host
                 #:command-line? #t
                 #:servlet-regexp #rx""))


(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))

(define IDLE-TIMEOUT-MINS 10)
(define IDLE-TIMEOUT-MILLIS (* IDLE-TIMEOUT-MINS 60 1000))

(define (service-session s)
  (define id (session-id s))
  (thread
   (lambda ()
     (define stdout-evt (push-output id (session-std-output s) 'stdout))
     (define stderr-evt (push-output id (session-error-output s) 'stderr))
     (define trace-evt (push-trace-evt id (session-trace-chan s)))
     (let loop ()
       (define timeout-evt (wait-for id))
       (when (sync stdout-evt stderr-evt trace-evt timeout-evt)
         (loop)))
     (log-sandbox-server-info "~a: Service thread for session ~a terminating" (timestamp) id))))

(define (push-output id port type)
  (handle-evt port
              (lambda (_p)
                (log-sandbox-server-info "~a: Reading output from ~a for session ~a" (timestamp) type id)
                (define out (read-string (pipe-content-length port) port))
                (define seq-no (next-seq-no! id type))
                (log-sandbox-server-info "~a: Sending session ~a output ~a on ~a" (timestamp) id seq-no type)
                (define url (format "/api/sessions/~a/output" id))
                (define msg (hash 'type (~a type) 'data out 'seq_no seq-no))
                (and (post-http! url msg)
                     (not (eof-object? out))))))

(define TRACE-TYPE 'trace)
(define (push-trace-evt id trace-chan)
  (handle-evt trace-chan
              (lambda (evt)
                (define seq-no (next-seq-no! id TRACE-TYPE))
                (log-sandbox-server-info "~a: Sending session ~a trace step ~a" (timestamp) id seq-no)
                (define url (format "/api/sessions/~a/output" id))
                (define msg (hash 'type (~a TRACE-TYPE) 'data (notification->json evt) 'seq_no seq-no))
                (post-http! url msg))))

(define (next-seq-no! id type)
  (define the-session (hash-ref session-envs id))
  (define nos (active-session-next-seq-nos the-session))
  (define next-seq (hash-ref nos type))
  (hash-set! session-envs
             id
             (struct-copy active-session the-session [next-seq-nos (hash-update nos type add1)]))
  next-seq)


(define (post-http! url data)
  (define conn? (http-conn-open (phx-host)
                                #:ssl? #f
                                #:port (phx-port)))
  (if conn?
      (begin
        (http-conn-send! conn?
                         url
                         #:method "POST"
                         #:headers (list "Content-Type: application/json")
                         #:data (jsexpr->string data))
        (http-conn-close! conn?))
      (log-sandbox-server-warning "~a: Unable to connect to ~a:~a" (timestamp) (phx-host) (phx-port))))

(define (wait-for id)
  (define deadline (or (deadline-for id) (current-inexact-milliseconds)))
  (handle-evt (alarm-evt deadline)
              (lambda (_)
                (cond
                  [(past-deadline? id)
                  (log-sandbox-server-info "~a: session ~a is idle" (timestamp) id)
                  (notify-idle! id)
                  (terminate! id)
                  (hash-remove! session-envs id)
                  #f]
                  [else
                   #t]))))

(define (deadline-for id)
  (define the-session (hash-ref session-envs id #f))
  (and the-session
       (+ (active-session-last-activity the-session) IDLE-TIMEOUT-MILLIS)))

(define (past-deadline? id)
  (define current-deadline (deadline-for id))
  (or (not current-deadline)
      (< current-deadline (current-inexact-milliseconds))))

(define (terminate! id)
  (define the-session (hash-ref session-envs id #f))
  (when the-session
    (kill-session (active-session-session the-session))))

(define (notify-idle! id)
  (define url (format "/api/sessions/~a/terminate" id))
  (define msg (hash 'reason "idle"))
  (post-http! url msg))
