#lang racket

(provide run-server)

(require "session.rkt"
         racket/date
         net/url
         json
         web-server/servlet-env
         web-server/http/json
         web-server/dispatch
         web-server/http/request-structs)

(define-logger sandbox-server)

(define session-envs (make-hash))

(define (create-session id)
  (hash-set! session-envs id
             (new-session #:id id)))

(define (evaluate-code id code)
  (define env (hash-ref session-envs id #f))
  (if env
      (with-handlers ([exn:fail?
                       (λ (e) (format "Error: ~a" (exn-message e)))])
        (session-eval env code))
      "Error: Session not found"))

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

(define (handle-unknown req)
  (response/jsexpr (hash 'status "Unknown command")))

(define-values (dispatch-request format-url)
  (dispatch-rules
   [("new")
    #:method "post"
    handle-new-session]
   [("submit")
    #:method "post"
    handle-submit]
   [else
    handle-unknown]))

(define (handle-request req)
  (log-sandbox-server-info "~a: Received ~a request to ~a" (timestamp) (request-method req) (url->string (request-uri req)))
  (dispatch-request req))

(define (run-server #:port [port 4001])
  (log-sandbox-server-info "~a: Server running on port ~a" (timestamp) port)
  (serve/servlet handle-request
                 #:port port
                 #:command-line? #t
                 #:servlet-regexp #rx""))


(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))
