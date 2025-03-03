#lang racket

(provide run-server
         phx-host
         phx-port)

(require "session.rkt"
         "trace-combiner.rkt"
         racket/date
         net/url
         json
         web-server/servlet-env
         web-server/http/json
         web-server/dispatch
         web-server/http/request-structs
         net/http-client)

(define-logger sandbox-server)

(struct active-session (session
                        last-activity
                        next-seq-nos
                        output-buffer
                        buffer-needs-flush?
                        buffer-last-flush)
  #:transparent)

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
                                                   TRACE-TYPE 0)
                                             (hash)
                                             #f
                                             (current-inexact-milliseconds)))
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

(define BUFFER-FLUSH-DELAY 100) ; milliseconds to wait before flushing buffer

(define (service-session s)
  (define id (session-id s))
  (thread
   (lambda ()
     (define stdout-evt (push-output id (session-std-output s) 'stdout))
     (define stderr-evt (push-output id (session-error-output s) 'stderr))
     (define trace-evt (push-trace-evt s))
     (let loop ()
       (define timeout-evt (wait-for id))
       (define flush-evt (check-flush-buffer id))
       (when (sync stdout-evt stderr-evt trace-evt timeout-evt flush-evt)
         (loop)))
     (log-sandbox-server-info "~a: Service thread for session ~a terminating" (timestamp) id))))

(define (check-flush-buffer id)
  (define the-session (hash-ref session-envs id #f))
  (cond
    [(and the-session
          (active-session-buffer-needs-flush? the-session))
     (define deadline (+ (active-session-buffer-last-flush the-session) BUFFER-FLUSH-DELAY))
     (handle-evt (alarm-evt deadline)
                 (lambda (_e) (flush-buffer! id)))]
    [else
     never-evt]))

(define (buffer-output! id type data)
  (define the-session (hash-ref session-envs id #f))
  (when the-session
    (define buffer (active-session-output-buffer the-session))
    (define seq-no (next-seq-no! id type))

    (define next-buffer (hash-update buffer
                                     type
                                     (lambda (existing)
                                       (append existing (list (cons seq-no data))))
                                     '()))

    (hash-set! session-envs
              id
              (struct-copy active-session (hash-ref session-envs id)
                           [output-buffer next-buffer]
                           [buffer-needs-flush? #t]))))

(define (flush-buffer! id)
  (define the-session (hash-ref session-envs id #f))
  (when the-session
    (define buffer (active-session-output-buffer the-session))

    ; Only send if there's data
    (unless (hash-empty? buffer)
      (log-sandbox-server-info "~a: Flushing output buffer for session ~a" (timestamp) id)

      ; Create a batch message with all buffered outputs
      (define batch-data
        (for/list ([(type entries) (in-hash buffer)])
          (hash 'type (~a type)
                'entries (for/list ([entry (in-list entries)])
                           (hash 'seq_no (car entry)
                                 'data (cdr entry))))))

      ; Send the batch
      (define url (format "/api/sessions/~a/output" id))
      (post-http! url (hash 'outputs batch-data)))

    (hash-set! session-envs
               id
               (struct-copy active-session the-session
                            [output-buffer (hash)]
                            [buffer-needs-flush? #f]
                            [buffer-last-flush (current-inexact-milliseconds)]))))

(define (push-output id port type)
  (handle-evt port
              (lambda (_p)
                (log-sandbox-server-info "~a: Reading output from ~a for session ~a" (timestamp) type id)
                (define out (read-string (pipe-content-length port) port))
                (buffer-output! id type out)
                (not (eof-object? out)))))

(define TRACE-TYPE 'trace)
(define (push-trace-evt s)
  (define id (session-id s))
  (handle-evt (session-trace-chan s)
              (lambda (evt)
                (define json (notification->json evt))
                (buffer-output! id TRACE-TYPE json)
                #t)))

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
    (when (active-session-buffer-needs-flush? the-session)
      (flush-buffer! id)) ; Flush any remaining buffered output
    (kill-session (active-session-session the-session))))

(define (notify-idle! id)
  (define url (format "/api/sessions/~a/terminate" id))
  (define msg (hash 'reason "idle"))
  (post-http! url msg))
