#lang racket

(provide new-session
         (struct-out session)
         get-session-output
         get-session-error-output
         session-alive?
         kill-session
         session-eval
         call-in-session-context
         flush-session
         session-memory-usage)

(require "tracing.rkt"
         racket/sandbox
         racket/async-channel)

(module+ test
  (require rackunit
           "utils.rkt"))

(define PIPE-BUFFER-SIZE (* 64 1024))
(define DEFAULT-SANDBOX-MEMORY-LIMIT-MB 30)
(define DEFAULT-INTERACTION-MEMORY-LIMIT-MB 4)
(define DEFAULT-INTERACTION-TIME-LIMIT-S 1)

;; a Session is a (session ID Procedure InputPort InputPort AsyncChannel Box)
(struct session (id sandbox-eval std-output error-output trace-chan source-name-box) #:transparent)

(define (new-session #:id [id #f] #:memory [memory-limit DEFAULT-SANDBOX-MEMORY-LIMIT-MB])
  (set! id (or id (gensym 'session)))
  (define-values (std-in std-out) (make-pipe PIPE-BUFFER-SIZE))
  (define-values (err-in err-out) (make-pipe PIPE-BUFFER-SIZE))

  (define source-name-box (box (format "session-~a" id)))

  (define evaluator
    (parameterize ([sandbox-output std-out]
                   [sandbox-error-output err-out]
                   [sandbox-memory-limit memory-limit]
                   [sandbox-eval-limits (list DEFAULT-INTERACTION-TIME-LIMIT-S
                                              DEFAULT-INTERACTION-MEMORY-LIMIT-MB)]
                   [sandbox-eval-handlers (list #f
                                                call-with-killing-threads)]
                   [sandbox-reader (make-sandbox-reader source-name-box)]
                   [sandbox-namespace-specs (list sandbox-make-namespace
                                                  'syndicate-sandbox/tracing
                                                  'syndicate/trie
                                                  '(submod syndicate/actor implementation-details))]
                   [current-logger (make-logger)])
      (make-evaluator 'racket
                      #:requires (list '(submod syndicate-sandbox/session sandbox-init)
                                       'syndicate-sandbox/lang
                                       'syndicate/drivers/timestate)
                      '(void (init-session)))))
  (define trace-chan (evaluator '(let () (local-require syndicate-sandbox/tracing) (current-trace-channel))))
  (session id evaluator std-in err-in trace-chan source-name-box))

(define ((make-sandbox-reader source-name-box) _sandbox-src)
  (define name (unbox source-name-box))
  (for/list ([x (in-producer (lambda () (read-syntax name)) eof)])
    x))

(module sandbox-init racket/base
  (provide init-session)

  (require syndicate/drivers/repl
           (only-in syndicate run-ground)
           (only-in syndicate/store with-store)
           (only-in syndicate/trace current-trace-procedures)
           racket/async-channel
           "tracing.rkt"
           "trace-combiner.rkt")
  (define (init-session)
    (current-trace-channel (make-async-channel))
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
      (define trace-proc (make-combined-tracer))
      (thread (lambda ()
                (parameterize ([current-endpoint-notification-handler trace-proc])
                  (with-store ([current-trace-procedures (cons trace-proc (current-trace-procedures))])
                    (run-ground (boot-repl #:when-ready ready-chan))))))
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

(define (session-eval s input [source-name #f])
  (define unique-source-name
    (or source-name
        (format "session-eval-~a-~a"
                (session-id s)
                (current-inexact-milliseconds))))
  (set-box! (session-source-name-box s) unique-source-name)
  ((session-sandbox-eval s) input))

(define (call-in-session-context s f)
  (call-in-sandbox-context (session-sandbox-eval s) f))

(define (flush-session s)
  (void (get-session-output s) (get-session-error-output s)))

(define (session-memory-usage s)
  (let* ([evaluator (session-sandbox-eval s)]
         [custodian (get-user-custodian evaluator)])
    (current-memory-use custodian)))

(module+ test
  (require rackunit/text-ui)
  (void
   (run-tests
    (test-suite
     "Sandbox tests"
     #:before collect-garbage
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
       (sleep 0.1)
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
       (session-eval s '(require (only-in racket [sleep rkt:sleep])))
       (define code `(begin (rkt:sleep ,(add1 DEFAULT-INTERACTION-TIME-LIMIT-S)) 'done))
       (check-exn exn:fail:resource?
                  (lambda () (session-eval s code)))
       (check-exn #rx"out of time"
                  (lambda () (session-eval s code))))

     (test-case
         "sandbox kills created threads"
       (define s (new-session))
       (session-eval s '(require (only-in racket [sleep rkt:sleep])))
       (define t (session-eval s '(thread (lambda () (rkt:sleep 10)))))
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
                  (lambda () (session-eval s '(system* "echo" "hi")))))

     (test-case
         "syndicate-repl logging goes to sandbox stderr"
       (define s (new-session))
       (sleep 0.1)
       (define output (get-session-error-output s))
       (check-true (regexp-match? #rx"syndicate-repl:"
                                  output)
                   output))

     (test-case
         "receive trace events from session"
       (define s (new-session))
       (sleep 0.1)
       (define evt (async-channel-try-get (session-trace-chan s)))
       (check-true (notification? evt)))

     (test-case
         "trace events are all serializable to json"
       (define s (new-session))
       (session-eval s "(spawn (for ([i (in-range 3)]) (react (field [x i]) (assert (x)))))")
       (sleep 1/4)
       (local-require "trace-combiner.rkt")
       (let loop ()
         (define evt (async-channel-try-get (session-trace-chan s)))
         (when evt
           (check-not-exn (lambda () (notification->json evt)))
           (loop))))

     (test-case
         "repl log doesn't cross talk between sessions"
       (define s1 (new-session))
       (sleep 1/10)
       (define s2 (new-session))
       (sleep 1/10)
       (check-false (string-contains? (get-session-error-output s1) (get-session-error-output s2))))

     (test-case
         "source locations reflect different session interactions"
       (define s1 (new-session))
       (session-eval s1 "(spawn (assert 'hello))" "source-1")
       (session-eval s1 "(spawn (assert 'hello))" "source-2")
       (sleep 1/10)
       (define actors (notification-detail (last (filter actors-notification? (channel->list (session-trace-chan s1))))))
       (define srclocs
         (for*/list ([ad (in-hash-values actors)]
                     [facet-detail (in-hash-values (actor-detail-facets ad))]
                     [ep (in-list (facet-eps facet-detail))])
           (endpoint-src ep)))
       (check-equal? (length srclocs) 2)
       (check-not-equal? (first srclocs)
                         (second srclocs))
       (check-equal? (list->set (map srcloc-source srclocs))
                     (set "source-1" "source-2")))

     (test-case
         "sandbox evaluates multiple expressions with each interaction"
       (define s (new-session))
       (define r (session-eval s "(define x 5) (+ x 2)"))
       (check-equal? r 7)))
    )))
