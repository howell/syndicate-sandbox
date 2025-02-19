#lang racket/base

(provide (all-from-out "tracing-facet-syntax.rkt")
         (all-from-out syndicate/interactive-lang))

(require (only-in "tracing-facet-syntax.rkt"
                  assert
                  on-stop
                  begin/dataflow
                  stop-when
                  stop-when-true
                  during

                  field
                  #;define-field
                  define/query-value
                  define/query-set
                  define/query-hash
                  define/query-hash-set
                  define/query-count))

(require (except-in syndicate/interactive-lang
                    #%module-begin
                    assert
                    on-stop
                    begin/dataflow
                    stop-when
                    stop-when-true
                    during

                    field
                    #;define-field
                    define/query-value
                    define/query-set
                    define/query-hash
                    define/query-hash-set
                    define/query-count
                    ))
