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
         define/query-count

         current-endpoint-notification-handler

         (struct-out endpoint-notification)
         endpoint-notification->json)

(require (prefix-in synd: syndicate/actor-lang)
         (prefix-in repl: syndicate/interactive-lang)
         (submod syndicate/actor implementation-details)
         syntax/parse/define
         (for-syntax racket/syntax
                     racket/match
                     syntax/stx
                     racket/syntax-srcloc))

(module+ test
  (require rackunit)
  (require syndicate/test/test-dataspace))

(struct endpoint-notification (fid desc detail srcloc) #:transparent)

(define-for-syntax (quote-src stx)
  (match-define (srcloc src line col pos span) (syntax-srcloc stx))
  (quasisyntax/loc stx
    (srcloc #,src #,line #,col #,pos #,span)))

(define-syntax-parse-rule (define-tracing-endpoint nm:id (~optional nm-:id))
  #:with synd-name (or (attribute nm-) (format-id #'nm "synd:~a" #'nm))
  #:with src (quote-src this-syntax)
  (define-syntax-parse-rule (nm . body)
    #:with the-ep this-syntax
    (begin
      (associate-endpoint! 'the-ep src)
      (synd-name . body))))

(define-simple-macro (define-tracing-endpoints nm:id ...+)
  (begin
    (define-tracing-endpoint nm)
    ...))

(define-tracing-endpoint assert repl:assert)

(define-tracing-endpoints
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
  #:with src (quote-src this-syntax)
  (define-syntax-parse-rule (nm field-nm:id . body)
    (begin
      (synd-name field-nm . body)
      (associate-field! field-nm src))))

(define-tracing-query define/query-value)
(define-tracing-query define/query-set)
(define-tracing-query define/query-hash)
(define-tracing-query define/query-hash-set)
(define-tracing-query define/query-count)
(define-tracing-query define-field)

;; The current-endpoint-notification-handler is an (Optionof {EPType Any -> Any})
;; an EPType is one of
;;   - 'endpoint
;;   - 'field
(define current-endpoint-notification-handler (make-parameter #f))

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
                           (endpoint-notification (? list?) 'field (field-handle (field-descriptor 'breakfast _)) (? srcloc?)))))))

  (test-case "simple query definition"
    (with-recording-handler store
      (with-test-dataspace [(synd:spawn (define/query-set brekkers (list 'breakfast $v) v))]
        (check-true (asserted? (synd:observe (list 'breakfast synd:?))))
        (check-match store
                     (list (endpoint-notification (? list?) 'field (field-handle (field-descriptor 'brekkers _)) (? srcloc?)))))))

  (test-case "assert still works with repl"
    (with-recording-handler store
      (with-test-dataspace [#f]
        (assert 'momma)
        (check-true (asserted? 'momma))
        (check-match store
                     (list (endpoint-notification '() 'endpoint (== '(assert 'momma)) (? srcloc?))))))))


;; EndpointNotification -> JSExpr
(define (endpoint-notification->json n)
  (hash 'facet_id (~a (endpoint-notification-fid n))
        'type (~a (endpoint-notification-desc n))
        'detail (endpoint-detail->json (endpoint-notification-detail n))
        'location (srcloc->json (endpoint-notification-srcloc n))))

;; Any -> JSExpr
;; Convert endpoint detail to JSON representation
(define (endpoint-detail->json detail)
  (cond
    [(syntax? detail)
     (~a (syntax->datum detail))]
    [(field-handle? detail)
     (field-handle->json detail)]
    [else (~v detail)]))

;; FieldHandle -> JSExpr
(define (field-handle->json fh)
  (match-define (field-handle (field-descriptor name _)) fh)
  (hash 'field_name (~a name)))

;; Srcloc -> JSExpr
(define (srcloc->json loc)
  (match-define (srcloc source line col pos span) loc)
  (hash 'source (~a source)
        'line line
        'column col
        'position pos
        'span span))

(module+ test
  (test-case "endpoint-notification->json"
    (define sample-srcloc (srcloc "test.rkt" 1 2 3 10))
    (define sample-notification
      (endpoint-notification '(facet1) 'endpoint '(assert 'hello) sample-srcloc))

    (check-equal?
     (endpoint-notification->json sample-notification)
     (hash 'facet_id "(facet1)"
           'type "endpoint"
           'detail "'(assert 'hello)"
           'location (hash 'source "test.rkt"
                          'line 1
                          'column 2
                          'position 3
                          'span 10)))

    (check-equal?
     (endpoint-notification->json
      (endpoint-notification '(facet2) 'field
                            (field-handle (field-descriptor 'breakfast #f))
                            sample-srcloc))
     (hash 'facet_id "(facet2)"
           'type "field"
           'detail (hash 'field_name "breakfast")
           'location (hash 'source "test.rkt"
                          'line 1
                          'column 2
                          'position 3
                          'span 10)))

    (check-equal?
     (endpoint-notification->json
      (endpoint-notification '(facet3) 'endpoint #'(+ 1 2) sample-srcloc))
     (hash 'facet_id "(facet3)"
           'type "endpoint"
           'detail "(+ 1 2)"
           'location (hash 'source "test.rkt"
                          'line 1
                          'column 2
                          'position 3
                          'span 10))))

  (test-case "endpoint-detail->json"
    (check-equal? (endpoint-detail->json '(hello world))
                  "'(hello world)")
    (check-equal? (endpoint-detail->json #'(+ 1 2))
                  "(+ 1 2)")
    (check-equal? (endpoint-detail->json
                   (field-handle (field-descriptor 'test #f)))
                  (hash 'field_name "test")))

  (test-case "field-handle->json"
    (check-equal? (field-handle->json
                   (field-handle (field-descriptor 'breakfast #f)))
                  (hash 'field_name "breakfast")))

  (test-case "srcloc->json"
    (check-equal? (srcloc->json (srcloc "test.rkt" 1 2 3 10))
                  (hash 'source "test.rkt"
                        'line 1
                        'column 2
                        'position 3
                        'span 10))))
