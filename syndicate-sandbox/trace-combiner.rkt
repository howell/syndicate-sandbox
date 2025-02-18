#lang racket

(provide make-combined-tracer)

#|
Receive updates from both the dataspace trace and facet endpoints trace to build
an association between each actor's facets and endpoints
|#

(require "tracing.rkt"
         "tracing-facet-syntax.rkt"
         "dataspace-trace-integrator.rkt"
         racket/async-channel
         syndicate/trace)

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
