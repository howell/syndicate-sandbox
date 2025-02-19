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
  ;; because the actor's behavior and state are initialized BEFORE the spawn trace event, we need to keep these around
  (define pending-endpoint-evts '())
  (define (on-event evt)
    (define-values (next-ds next-facets next-pending-evts)
      (receive-update curr-ds curr-facets pending-endpoint-evts evt))
    (set! pending-endpoint-evts next-pending-evts)
    (unless (eq? next-ds curr-ds)
      (async-channel-put ch next-ds)
      (set! curr-ds next-ds))
    (unless (eq? next-facets curr-facets)
      (async-channel-put ch next-facets)
      (set! curr-facets next-facets)))
  on-event)

;; Dataspace ActorEnv TraceEvent -> {Values Dataspace ActorEnv}
(define (receive-update curr-ds curr-facets pending-endpoint-evts evt)
  (cond
    [(and (not (empty? pending-endpoint-evts))
          (trace-notification? evt)
          (equal? 'spawn (trace-notification-type evt)))
     (define spawned-pid (spacetime-space (trace-notification-sink evt)))
     (values (apply-notification curr-ds evt)
             (apply-pending-evts curr-facets spawned-pid pending-endpoint-evts)
             '())]
    [(trace-notification? evt)
     (values (apply-notification curr-ds evt)
             curr-facets
             pending-endpoint-evts)]
    [(and (endpoint-notification? evt)
          (active-actor-id curr-ds))
     (values curr-ds
             (associate-endpoint curr-facets (active-actor-id curr-ds) evt)
             pending-endpoint-evts)]
    [else
     (values curr-ds curr-facets (cons evt pending-endpoint-evts))]))

;; ActorEnv PID EndpointNotification -> ActorEnv
(define (associate-endpoint env pid evt)
  (define (add-ep existing-facets)
    (hash-update existing-facets
                 (endpoint-notification-fid evt)
                 (lambda (existing-eps) (cons evt existing-eps))
                 '()))
  (hash-update env pid add-ep (hash)))

(define (apply-pending-evts env pid evts)
  (for/fold ([env env])
            ([evt (in-list evts)])
    (associate-endpoint env pid evt)))

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
  (require (submod syndicate/actor implementation-details)
           syndicate/store
           syndicate/test/test-dataspace
           "lang.rkt")

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
                                                         'span 10)))))))))

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
                                             'span 10)))))))

  (define (channel->list ch)
    (define r (async-channel-try-get ch))
    (if r
        (cons r (channel->list ch))
        '()))

  (test-case "pending endpoint events are applied to spawned actor"
    (define test-ch (make-async-channel))
    (define on-evt (make-combined-tracer test-ch))
    (parameterize ([current-endpoint-notification-handler on-evt])
      (with-store [(current-trace-procedures (current-trace-procedures (cons on-evt (current-trace-procedures))))]
        (with-test-dataspace []
          (void (channel->list test-ch))
          (spawn (assert 'hello))
          (sleep 1/4)
          (define evts (channel->list test-ch))
          (pretty-display evts)
          (define actor-env? hash?)
          (define env-evt (findf actor-env? evts))
          (check-not-false env-evt)
          (check-match env-evt
                       (hash '(2)
                             (hash '(4)
                                   (list (endpoint-notification '(4)
                                                                'endpoint
                                                                '(assert 'hello)
                                                                (? srcloc?)))))))))))
