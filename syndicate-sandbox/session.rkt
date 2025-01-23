#lang racket

(provide new-session
         (struct-out session)
         get-session-output
         get-session-error-output
         session-alive?
         kill-session
         session-eval
         flush-session
         session-memory-usage)

(require racket/sandbox
         racket/runtime-path)

(module+ test
  (require rackunit))

(define PIPE-BUFFER-SIZE (* 64 1024))
(define DEFAULT-SANDBOX-MEMORY-LIMIT-MB 30)
(define DEFAULT-INTERACTION-MEMORY-LIMIT-MB 4)
(define DEFAULT-INTERACTION-TIME-LIMIT-S 1)

;; a Session is a (session ID Procedure InputPort InputPort)
(struct session (id sandbox-eval std-output error-output) #:transparent)

(define (new-session #:id [id #f] #:memory [memory-limit DEFAULT-SANDBOX-MEMORY-LIMIT-MB])
  (set! id (or id (gensym 'session)))
  (define-values (std-in std-out) (make-pipe PIPE-BUFFER-SIZE))
  (define-values (err-in err-out) (make-pipe PIPE-BUFFER-SIZE))
  (define evaluator
    (parameterize ([sandbox-output std-out]
                   [sandbox-error-output err-out]
                   [sandbox-memory-limit memory-limit]
                   [sandbox-eval-limits (list DEFAULT-INTERACTION-TIME-LIMIT-S
                                              DEFAULT-INTERACTION-MEMORY-LIMIT-MB)]
                   [sandbox-eval-handlers (list #f
                                                call-with-killing-threads)]
                   [current-logger (make-logger)])
      (make-evaluator 'racket
                      #:requires (list '(submod syndicate-sandbox/session sandbox-init)
                                       'syndicate/drivers/timestate)
                      '(require (except-in syndicate/interactive-lang #%module-begin))
                      '(void (init-session)))))
  (session id evaluator std-in err-in))

(module sandbox-init racket/base
  (provide init-session)

  (require syndicate/drivers/repl
           (only-in syndicate run-ground)
           racket/async-channel
           racket/logging)
  (define (init-session)
    (let ([ready-chan (make-async-channel)])
      (thread (lambda ()
                (define receiver (make-log-receiver (current-logger)
                                                    'info 'syndicate-repl
                                                    'error #f))
                (async-channel-put ready-chan 'ok)
                (let loop ()
                  (sync (handle-evt receiver
                                    (lambda (v)
                                      (displayln (vector-ref v 1) (current-error-port)))))
                  (loop))))
      (async-channel-get ready-chan)
      (thread (lambda () (run-ground (boot-repl #:when-ready ready-chan))))
      (async-channel-get ready-chan)
      (repl-activate syndicate/drivers/timestate))))

(define (kill-session s)
  (kill-evaluator (session-sandbox-eval s)))

(define (read-from-sandbox input-port)
  (read-string (pipe-content-length input-port) input-port))

(define (get-session-output s)
  (read-from-sandbox (session-std-output s)))

(define (get-session-error-output s)
  (read-from-sandbox (session-error-output s)))

(define (session-alive? s)
  (evaluator-alive? (session-sandbox-eval s)))

(define (session-eval s input)
  ((session-sandbox-eval s) input))

(define (flush-session s)
  (void (get-session-output s) (get-session-error-output s)))

(define (session-memory-usage s)
  (let* ([evaluator (session-sandbox-eval s)]
         [custodian (get-user-custodian evaluator)])
    (current-memory-use custodian)))

(module+ test
  (test-case
      "can create a sandbox session"
    (parameterize ([current-custodian (make-custodian)])
      (check-true (session? (new-session)))))

  (test-case
      "can interact with syndicate repl in sandbox"
    (define s (new-session))
    (check-true (session-alive? s))
    (check-equal? (session-eval s '(do-assert 'hello))
                  'ok)
    (check-equal? (session-eval s '(begin (require syndicate/trie)
                                          (trie-key-set/single (do-query 'hello))))
                  (set 'hello))
    (flush-session s)
    (check-equal? (session-eval s '(spawn (on-start (display 'jeepers))))
                  'ok)
    (sleep 0.1)
    (check-equal? (get-session-output s)
                  "jeepers"))

  (test-case
      "can define and work with structs in the sandbox"
    (define s (new-session))
    (session-eval s '(struct dog (spots) #:transparent))
    (check-equal? (session-eval s '(do-assert (dog 14)))
                  'ok)
    (check-equal? (session-eval s '(do-query/set (dog (?!))))
                  (set 14)))

  (test-case
      "can get output from sandbox"
    (define s (new-session))
    (flush-session s)
    (check-equal? (get-session-output s) "")
    (session-eval s '(display 'worry))
    (check-equal? (get-session-output s) "worry")
    (check-equal? (get-session-output s) ""))

  (test-case
      "can get error output from sandbox"
    (define s (new-session))
    (flush-session s)
    (check-equal? "" (get-session-error-output s))
    (session-eval s '(display 'worry (current-error-port)))
    (check-equal? (get-session-error-output s) "worry")
    (check-equal? (get-session-error-output s) ""))

  (test-case
      "sandbox raises when evaluated expression throws an exception"
    (define s (new-session))
    (check-exn exn:fail?
               (lambda () (session-eval s "(error 'woops)"))))

  (test-case
      "sandbox raises an exception when given invalid syntax"
    (define s (new-session))
    (check-exn exn:fail?
               (lambda () (session-eval s "(+ 1 3"))))

  (test-case
      "sandbox raises an exception when memory limit is reached"
    (define s (new-session))
    (define allocator '(let loop ([l (list)])
                         (loop (cons 1 l))))
    (check-exn #rx"out of memory|out-of-memory"
               (lambda () (session-eval s allocator))))

  (test-case
      "sandbox memory limit with make-bytes allocator"
    (define s (new-session))
    (define allocator `(for/list ([i (in-range ,(+ 4 DEFAULT-INTERACTION-MEMORY-LIMIT-MB))])
                         (collect-garbage)
                         (make-bytes 1000000)))
    (check-exn #rx"out of memory"
               (lambda () (session-eval s allocator))))

  (test-case
      "sandbox memory limit with set! allocator"
    (define s (new-session))
    (define allocator `(let ()
                        (define a '())
                        (for ([i (in-range ,(add1 DEFAULT-INTERACTION-MEMORY-LIMIT-MB))])
                          (set! a (cons (make-bytes 1000000) a))
                          (collect-garbage))))
    (check-exn #rx"out of memory"
               (lambda () (session-eval s allocator))))

  #;(test-case
      "sandbox memory limit with set! allocator, top level"
    (define s (new-session #:memory 2))
    (session-eval s '(define a (list)))
    #;(define allocator "(define a '())
                       (for ([i (in-range 100)])
                         (set! a (cons (make-bytes 1000000) a))
                         (collect-garbage))")
    (define allocator `(begin
                         (set! a (cons (make-bytes 1000000) a))
                         (collect-garbage)))
    (check-exn exn:fail:resource?
               (lambda () (for ([i (in-range 5)])
                            (session-eval s allocator)))))

  (test-case
      "sandbox enforces shallow time limit"
    (define s (new-session))
    (define code `(begin (sleep ,(add1 DEFAULT-INTERACTION-TIME-LIMIT-S)) 'done))
    (check-exn exn:fail:resource?
               (lambda () (session-eval s code)))
    (check-exn #rx"out of time"
               (lambda () (session-eval s code))))

  (test-case
      "sandbox kills created threads"
    (define s (new-session))
    (define t (session-eval s '(thread (lambda () (sleep 10)))))
    (sleep 0.1)
    (check-true (thread-dead? t)))

  (test-case
      "sandbox restricts file system access"
    (define s (new-session))
    (check-exn exn:fail?
               (lambda () (session-eval s '(directory-exists? "/"))))
    (check-exn #rx"`exists' access denied"
               (lambda () (session-eval s '(directory-exists? "/"))))
    (check-exn exn:fail?
               (lambda () (session-eval s '(make-temporary-file))))
    (check-exn #rx"`write' access denied"
               (lambda () (session-eval s '(make-temporary-file))))
    (check-exn exn:fail?
               (lambda () (session-eval s '(display-to-file 123 "./test.txt"))))
    (check-exn #rx"`write' access denied"
               (lambda () (session-eval s '(display-to-file 123 "./test.txt"))))
    (check-not-exn (lambda () (session-eval s '(file->string "../info.rkt"))))
    (check-not-exn (lambda () (session-eval s '(directory-list "../"))))
    (check-exn exn:fail?
               (lambda () (session-eval s '(directory-list "../../"))))
    (check-exn #rx"`read' access denied"
               (lambda () (session-eval s '(directory-list "../../"))))
    (check-exn exn:fail?
               (lambda () (session-eval s '(directory-list "/"))))
    (check-exn #rx"`read' access denied"
               (lambda () (session-eval s '(directory-list "/")))))

  (test-case
      "sandbox restricts network access"
    (define s (new-session))
    (check-exn exn:fail?
               (lambda () (session-eval s '(tcp-listen 4040))))
    (check-exn #rx"network access denied"
               (lambda () (session-eval s '(tcp-listen 4040))))
    (check-exn exn:fail?
               (lambda () (session-eval s '(tcp-connect "localhost" 4040))))
    (check-exn #rx"network access denied"
               (lambda () (session-eval s '(tcp-connect "localhost" 4040)))))

  (test-case
      "sandbox restricts system calls"
    (define s (new-session))
    (check-exn exn:fail?
               (lambda () (session-eval s '(system "echo hi"))))
    (check-exn #rx"`execute' access denied"
               (lambda () (session-eval s '(system "echo hi"))))
    (check-exn exn:fail?
               (lambda () (session-eval s '(system* "echo" "hi"))))
    (check-exn #rx"`execute' access denied"
               (lambda () (session-eval s '(system* "echo" "hi"))))))


