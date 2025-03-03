#lang racket

(provide assert
         on
         on-stop
         begin/dataflow
         stop-when
         stop-when-true
         during

         field
         define-field
         define/query-value
         define/query-set
         define/query-hash
         define/query-hash-set
         define/query-count

         current-endpoint-notification-handler
         )

(require "tracing.rkt"
         (prefix-in synd: syndicate/actor-lang)
         (prefix-in repl: syndicate/interactive-lang)
         (prefix-in synd: (submod syndicate/actor implementation-details))
         syntax/parse/define
         (for-syntax racket/syntax
                     racket/match
                     syntax/stx
                     racket/syntax-srcloc))

(module+ test
  (require rackunit)
  (require syndicate/test/test-dataspace))

(define-for-syntax (quote-src stx)
  (match-define (srcloc src line col pos span) (syntax-srcloc stx))
  (quasisyntax/loc stx
    (srcloc #,src #,line #,col #,pos #,span)))

(define-syntax-parse-rule (define-tracing-endpoint nm:id (~optional nm-:id))
  #:with synd-name (or (attribute nm-) (format-id #'nm "synd:~a" #'nm))
  (define-syntax-parse-rule (nm . body)
    #:with the-ep this-syntax
    #:with src (quote-src this-syntax)
    (begin
      (associate-endpoint! 'the-ep src)
      (synd-name . body))))

(define-simple-macro (define-tracing-endpoints nm:id ...+)
  (begin
    (define-tracing-endpoint nm)
    ...))

(define-tracing-endpoint assert repl:assert)

(define-tracing-endpoints
  on
  on-stop
  begin/dataflow
  stop-when
  stop-when-true
  during)

(define-syntax-parse-rule (field [nm:id v0] ...)
  #:with (src ...) (stx-map quote-src #'(nm ...))
  (begin
    (synd:field [nm v0] ...)
    (associate-field! nm src)
    ...))

(define-syntax-parse-rule (define-tracing-query nm:id)
  #:with synd-name (format-id #'nm "synd:~a" #'nm)
  (define-syntax-parse-rule (nm field-nm:id . body)
    #:with src (quote-src this-syntax)
    (begin
      (synd-name field-nm . body)
      (associate-field! field-nm src))))

(define-tracing-query define/query-value)
(define-tracing-query define/query-set)
(define-tracing-query define/query-hash)
(define-tracing-query define/query-hash-set)
(define-tracing-query define/query-count)
(define-tracing-query define-field)

(define (associate-endpoint! ep src)
  (when (current-endpoint-notification-handler)
    ((current-endpoint-notification-handler) (endpoint-notification (synd:current-facet-id)
                                                          'endpoint
                                                          ep
                                                          src))))

(define (associate-field! nm src)
  (when (current-endpoint-notification-handler)
    ((current-endpoint-notification-handler) (endpoint-notification (synd:current-facet-id)
                                                          'field
                                                          nm
                                                          src))))

(module+ test
  (require racket/port)

  (define-syntax-parse-rule (with-recording-handler store:id body ...+)
    (let ([store '()])
      (parameterize ([current-endpoint-notification-handler (lambda (evt)
                                                              (printf "Endpoint Event: ~a\n" evt)
                                                              (set! store (cons evt
                                                                                store)))])
        body ...)))

  (test-case "assert associates endpoint and facet"
    (with-recording-handler store
      (with-test-dataspace [(synd:spawn (assert 'hello))]
        (check-true (asserted? 'hello))
        (check-match store
                     (list (endpoint-notification (? list?) 'endpoint (== '(assert 'hello)) (? srcloc?)))))))

  (test-case "field associates field-handle with facet"
    (with-recording-handler store
      (with-test-dataspace [(synd:spawn (field [breakfast 'toast])
                                        (assert (breakfast)))]
        (check-true (asserted? 'toast))
        (check-match store
                     (list (endpoint-notification (? list?) 'endpoint (== '(assert (breakfast))) (? srcloc?))
                           (endpoint-notification (? list?) 'field (synd:field-handle (synd:field-descriptor 'breakfast _)) (? srcloc?)))))))

  (test-case "simple query definition"
    (with-recording-handler store
      (with-test-dataspace [(synd:spawn (define/query-set brekkers (list 'breakfast $v) v))]
        (check-true (asserted? (synd:observe (list 'breakfast synd:?))))
        (check-match store
                     (list (endpoint-notification (? list?) 'field (synd:field-handle (synd:field-descriptor 'brekkers _)) (? srcloc?)))))))

  (test-case "assert still works with repl"
    (with-recording-handler store
      (with-test-dataspace [#f]
        (assert 'momma)
        (check-true (asserted? 'momma))
        (check-match store
                     (list (endpoint-notification '() 'endpoint (== '(assert 'momma)) (? srcloc?)))))))

  (test-case "srcloc corresponds to use site and contains correct source"
    (with-recording-handler store
      (with-test-dataspace [(synd:spawn (assert 'test-value))]
        (check-true (asserted? 'test-value))
        (match store
          [(list (endpoint-notification _facet-id 'endpoint _expr (srcloc src _line _col pos span)))
           ;; Try to read the source file and verify the content
           (when (and src (file-exists? src))
             (define file-content
               (parameterize ([port-count-lines-enabled #t])
               (call-with-input-file src
                 (λ (in)
                   (file-position in (sub1 pos))
                   (read-string span in)))))
             (check-match file-content (regexp #rx"assert.*'test-value")))])))))
