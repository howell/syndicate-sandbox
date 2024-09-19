#lang racket

(provide new-session
         (struct-out session)
         get-session-output
         get-session-error-output
         session-alive?
         kill-session
         session-eval
         flush-session)

(require racket/sandbox)

(module+ test
  (require rackunit))

(define PIPE-BUFFER-SIZE (* 64 1024))
(define DEFAULT-MEMORY-LIMIT (* 16 1024 1024))

;; a Session is a (session ID Procedure InputPort InputPort)
(struct session (id sandbox-eval std-output error-output) #:transparent)

(define (new-session #:memory [memory-limit DEFAULT-MEMORY-LIMIT])
  (define id (gensym 'session))
  (define-values (std-in std-out) (make-pipe PIPE-BUFFER-SIZE))
  (define-values (err-in err-out) (make-pipe PIPE-BUFFER-SIZE))
  (define evaluator
    (parameterize ([sandbox-output std-out]
                   [sandbox-error-output err-out]
                   [sandbox-memory-limit memory-limit])
      (make-evaluator 'syndicate/lang
                      '(require syndicate/drivers/repl
                                racket
                                racket/async-channel)
                      '(let ([ready-chan (make-async-channel)])
                         (thread (lambda () (run-ground (boot-repl #:when-ready ready-chan))))
                         (async-channel-get ready-chan)))))
  (session id evaluator std-in err-in))

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
                  (set 'hello)))

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
    (check-exn exn:fail:resource?
               (lambda () (session-eval s allocator)))
    (check-exn #rx"out of memory"
               (lambda () (session-eval s allocator))))

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
