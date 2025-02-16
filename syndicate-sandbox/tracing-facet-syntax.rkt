#lang racket

(provide assert
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
         define/query-count)

(require (prefix-in synd: syndicate/actor-lang)
         (submod syndicate/actor implementation-details)
         syntax/parse/define
         (for-syntax racket/syntax))

(module+ test
  (require rackunit)
  (require syndicate/test/test-dataspace))

(define-syntax-parse-rule (define-tracing-endpoint nm:id)
  #:with synd-name (format-id #'nm "synd:~a" #'nm)
  (define-syntax-parse-rule (nm . body)
    #:with the-ep this-syntax
    (begin
      (associate-endpoint! 'the-ep)
      (synd-name . body))))

(define-simple-macro (define-tracing-endpoints nm:id ...+)
  (begin
    (define-tracing-endpoint nm)
    ...))

(define-tracing-endpoints
  assert
  on-stop
  begin/dataflow
  stop-when
  stop-when-true
  during)

(define-syntax-parse-rule (field [nm:id v0] ...)
  (begin
    (synd:field [nm v0] ...)
    (associate-field! nm)
    ...))

(define-syntax-parse-rule (define-tracing-query nm:id)
  #:with synd-name (format-id #'nm "synd:~a" #'nm)
  (define-syntax-parse-rule (nm field-nm:id . body)
    (begin
      (synd-name field-nm . body)
      (associate-field! field-nm))))

(define-tracing-query define/query-value)
(define-tracing-query define/query-set)
(define-tracing-query define/query-hash)
(define-tracing-query define/query-hash-set)
(define-tracing-query define/query-count)
(define-tracing-query define-field)

;; The current-association-handler is an (Optionof {EPType Any -> Any})
;; an EPType is one of
;;   - 'endpoint
;;   - 'field
(define current-association-handler (make-parameter #f))

(define (associate-endpoint! ep)
  (printf "associate-endpoint! ~a\n" (current-association-handler))
  (when (current-association-handler)
    (printf "calling handler!\n")
    ((current-association-handler) 'endpoint ep)))

(define (associate-field! nm)
  (when (current-association-handler)
    ((current-association-handler) 'field nm)))

(module+ test
  (define-syntax-parse-rule (with-recording-handler store:id body ...+)
    (let ([store '()])
      (parameterize ([current-association-handler (lambda (type ep)
                                                    (define fid (synd:current-facet-id))
                                                    (printf "fid ~a has endpoint: ~a\n" fid ep)
                                                    (set! store (cons (list type fid ep)
                                                                      store)))])
        body ...)))

  (test-case "assert associates endpoint and facet"
    (with-recording-handler store
      (with-test-dataspace [(synd:spawn (assert 'hello))]
        (check-true (asserted? 'hello))
        (check-match store
                     (list (list 'endpoint (? list?) (== '(assert 'hello))))))))

  (test-case "field associates field-handle with facet"
    (with-recording-handler store
      (with-test-dataspace [(synd:spawn (field [breakfast 'toast])
                                        (assert (breakfast)))]
        (check-true (asserted? 'toast))
        (check-match store
                     (list (list 'endpoint (? list?) (== '(assert (breakfast))))
                           (list 'field (? list?) (field-handle (field-descriptor 'breakfast _))))))))

  (test-case "simple query definition"
    (with-recording-handler store
      (with-test-dataspace [(synd:spawn (define/query-set brekkers (list 'breakfast $v) v))]
        (check-true (asserted? (synd:observe (list 'breakfast synd:?))))
        (check-match store
                     (list (list 'field (? list?) (field-handle (field-descriptor 'brekkers _)))))))
    ))
