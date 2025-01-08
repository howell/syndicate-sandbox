#lang racket

(provide run-server
         phx-host
         phx-port)

(require "session.rkt"
         racket/date
         net/url
         json
         web-server/servlet-env
         web-server/http/json
         web-server/dispatch
         web-server/http/request-structs
         net/http-client)

(define-logger sandbox-server)

(struct active-session (session last-activity) #:transparent)

(define session-envs (make-hash))

(define phx-host (make-parameter (or (getenv "PHX_HOST") "localhost")))
(define phx-port (make-parameter (let ([p (getenv "PHX_PORT")])
                                   (if p
                                       (string->number p)
                                       4000))))


(define (create-session id)
  (define s (new-session #:id id))
  (hash-set! session-envs id (active-session s (current-inexact-milliseconds)))
  (service-session s))

(define (evaluate-code id code)
  (define s (hash-ref session-envs id #f))
  (if s
      (with-handlers ([exn:fail?
                       (λ (e) (format "Error: ~a" (exn-message e)))])
        (begin
          (mark-activity! s)
          (session-eval (active-session-session s) code)))
      "Error: Session not found"))

(define (mark-activity! s)
  (hash-set! session-envs
             (session-id (active-session-session s))
             (active-session (active-session-session s) (current-inexact-milliseconds))))

(define (handle-new-session req)
  (define msg (bytes->jsexpr (request-post-data/raw req)))
  (log-sandbox-server-info "~a: Received new session request with body ~a" (timestamp) msg)
  (define id (hash-ref msg 'session_id))
  (create-session id)
  (response/jsexpr (hash 'status "ok")))

(define (handle-submit req)
  (define msg (bytes->jsexpr (request-post-data/raw req)))
  (log-sandbox-server-info "~a: Received code submission request with body ~a" (timestamp) msg)
  (define id (hash-ref msg 'session_id))
  (define code (hash-ref msg 'code))
  (response/jsexpr (hash 'status "ok" 'result (~a (evaluate-code id code)))))

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

(define IDLE-TIMEOUT-MILLIS (* 1000 60 5))

(define (service-session s)
  (define id (session-id s))
  (thread
   (lambda ()
     (define stdout-evt (push-output id (session-std-output s) 'stdout))
     (define stderr-evt (push-output id (session-error-output s) 'stderr))
     (let loop ()
       (define timeout-evt (wait-for id))
       
       (when (sync stdout-evt stderr-evt timeout-evt)
         (loop)))
     (log-sandbox-server-info "~a: Service thread for session ~a terminating" (timestamp) id))))

(define (push-output id port type)
  (handle-evt port
              (lambda (_p)
                (log-sandbox-server-info "~a: Reading output from ~a for session ~a" (timestamp) type id)
                (define out (read-string (pipe-content-length port) port))
                (and (post-http! id type out)
                     (not (eof-object? out))))))

(define (post-http! id type data)
  (log-sandbox-server-info "~a: Sending session ~a output on ~a" (timestamp) id type)
  (define conn? (http-conn-open (phx-host)
                               #:ssl? #f
                               #:port (phx-port)))
  (if conn?
      (begin
        (http-conn-send! conn?
                         (format "/api/sessions/~a" id)
                         #:method "POST"
                         #:headers (list "Content-Type: application/json")
                         #:data (jsexpr->string (hash 'type (~a type) 'data data)))
        (http-conn-close! conn?))
      (log-sandbox-server-info "~a: Unable to connect to ~a:~a" (timestamp) (phx-host) (phx-port))))

(define (wait-for id)
  (define the-session (hash-ref session-envs id #f))
  (define last-active (if the-session
                          (active-session-last-activity the-session)
                          (current-inexact-milliseconds)))
  (handle-evt (alarm-evt (+ last-active IDLE-TIMEOUT-MILLIS))
              (lambda (_)
                (log-sandbox-server-info "~a: session ~a is idle" (timestamp) id)
                (hash-remove! session-envs id)
                #f)))
