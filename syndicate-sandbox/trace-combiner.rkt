#lang racket

(provide make-combined-tracer
         actor-env->json)

#|
Receive updates from both the dataspace trace and facet endpoints trace to build
an association between each actor's facets and endpoints
|#

(require "tracing.rkt"
         "tracing-facet-syntax.rkt"
         "dataspace-trace-integrator.rkt"
         racket/async-channel
         syndicate/trace
         json)

(module+ test
  (require rackunit))

;; an ActorEnv is a (Hashof PID ActorDetail)
;; an ActorDetail is a (Hashof FID FacetDetail)
;; a FacetDetail is a (Listof EndpointDetail)
;; an EndpointDetail is a (List Symbol Any)

;; a TraceEvent is a TraceNotification or an EndpointNotification

(define (make-combined-tracer [ch (current-trace-channel)])
  (define curr-ds (dataspace (hash) #f '() '() #f))
  (define curr-facets (hash))
  (define (on-event evt)
    (define-values (next-ds next-facets) (receive-update curr-ds curr-facets evt))
    (unless (eq? next-ds curr-ds)
      (async-channel-put ch next-ds)
      (set! curr-ds next-ds))
    (unless (eq? next-facets curr-facets)
      (async-channel-put ch next-facets)
      (set! curr-facets next-facets)))
  on-event)

;; Dataspace ActorEnv TraceEvent -> {Values Dataspace ActorEnv}
(define (receive-update curr-ds curr-facets evt)
  (cond
    [(trace-notification? evt)
     (values (apply-notification curr-ds evt)
             curr-facets)]
    [(and (endpoint-notification? evt)
          (active-actor-id curr-ds))
     (values curr-ds
             (associate-endpoint curr-facets (active-actor-id curr-ds) evt))]
    [else
     (log-syndicate-trace-warning "Received unexpected trace event: ~a" evt)
     (values curr-ds curr-facets)]))

;; ActorEnv PID EndpointNotification -> ActorEnv
(define (associate-endpoint env pid evt)
  (define (add-ep existing-facets)
    (hash-update existing-facets
                 (endpoint-notification-fid evt)
                 (lambda (existing-eps) (cons evt existing-eps))
                 '()))
  (hash-update env pid add-ep (hash)))

;; ActorEnv -> JSExpr
(define (actor-env->json env)
  (for/list ([(pid facets) (in-hash env)])
    (hash 'actor_id (~a pid)
          'facets (facets->json facets))))

;; (Hashof FID FacetDetail) -> JSExpr
(define (facets->json facets)
  (for/list ([(fid endpoints) (in-hash facets)])
    (hash 'facet_id (~a fid)
          'endpoints (map endpoint-notification->json endpoints))))

(module+ test
  (require (submod syndicate/actor implementation-details))

  (test-case "actor-env->json"
    (define sample-srcloc (srcloc "test.rkt" 1 5 50 10))
    (define sample-env
      (hash '(actor1)
            (hash '(facet1)
                  (list (endpoint-notification '(facet1) 'endpoint '(assert 'hello) sample-srcloc)))))

    (check-equal?
     (actor-env->json sample-env)
     (list (hash 'actor_id "(actor1)"
                 'facets
                 (list (hash 'facet_id "(facet1)"
                             'endpoints
                             (list (hash 'facet_id "(facet1)"
                                         'type "endpoint"
                                         'detail "'(assert 'hello)"
                                         'location (hash 'source "test.rkt"
                                                         'line 1
                                                         'column 5
                                                         'position 50
                                                         'span 10))))))))

    (test-case "facets->json"
      (define sample-srcloc (srcloc "test.rkt" 1 5 50 10))
      (define sample-facets
        (hash '(facet1)
              (list (endpoint-notification '(facet1) 'endpoint '(assert 'hello) sample-srcloc)
                    (endpoint-notification '(facet1) 'field
                                           (field-handle (field-descriptor 'test #f))
                                           sample-srcloc))))

      (check-equal?
       (facets->json sample-facets)
       (list (hash 'facet_id "(facet1)"
                   'endpoints
                   (list (hash 'facet_id "(facet1)"
                               'type "endpoint"
                               'detail "'(assert 'hello)"
                               'location (hash 'source "test.rkt"
                                               'line 1
                                               'column 5
                                               'position 50
                                               'span 10))
                         (hash 'facet_id "(facet1)"
                               'type "field"
                               'detail (hash 'field_name "test")
                               'location (hash 'source "test.rkt"
                                               'line 1
                                               'column 5
                                               'position 50
                                               'span 10)))))))))
