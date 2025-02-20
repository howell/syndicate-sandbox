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
         (prefix-in synd: (submod syndicate/actor implementation-details))
         json)

(module+ test
  (require rackunit
           syndicate/store
           syndicate/test/test-dataspace
           (prefix-in stx: "lang.rkt")))

;; an ActorEnv is a (Hashof PID ActorDetail)
;; an ActorDetail is a (Hashof FID FacetDetail)

;; a FacetDetail is a (facet FID (Listof Field) (Listof Endpoint) (Setof FID))
(struct facet (id fields eps children) #:transparent)
(define (make-facet fid) (facet fid '() '() (set)))

;; a Field is a (field FieldHandle Any SrcLoc)
(struct field (handle val src) #:transparent)

;; an Endpoint is a (endpoint Any SrcLoc)
(struct endpoint (description src) #:transparent)

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
  (define fid (endpoint-notification-fid evt))
  (define (update-actor existing-facets)
    (hash-update existing-facets
                 fid
                 (lambda (fct) (add-notification fct evt))
                 (lambda () (make-facet fid))))
  (hash-update env pid update-actor (hash)))

;; Facet EndpointNotification -> Facet
(define (add-notification fct evt)
  (define adder
    (case (endpoint-notification-desc evt)
      [(endpoint) add-ep]
      [(field) add-field]))
  (adder fct
         (endpoint-notification-detail evt)
         (endpoint-notification-srcloc evt)))

(define (add-ep fct desc src)
  (struct-copy facet fct
               [eps (cons (endpoint desc src)
                          (facet-eps fct))]))

(define (add-field fct handle src)
  (struct-copy facet fct
               [fields (cons (field handle (handle) src)
                             (facet-fields fct))]))

(define (apply-pending-evts env pid evts)
  (for/fold ([env env])
            ([evt (in-list evts)])
    (associate-endpoint env pid evt)))

;; ActorDetail ActorState -> ActorDetail
;; Update the information associated with an actor's facets based on the observed state of the process
(define (update-process-state facets as)
  (define live-facets (synd:actor-state-facets as))
  (define field-table (synd:actor-state-field-table as))

  (define pruned-facets
    (for/hash ([(fid fct) (in-hash facets)]
               #:when (hash-has-key? live-facets fid))
      (values fid fct)))

  (for/hash ([(fid fct) (in-hash pruned-facets)])
    (values fid
            (let* ([live-facet (hash-ref live-facets fid)]
                   [with-children (struct-copy facet fct
                                               [children (synd:facet-children live-facet)])]
                   [with-fields (struct-copy facet with-children
                                             [fields (update-field-values (facet-fields fct)
                                                                          field-table)])])
              with-fields))))

;; (Listof Field) FieldTable -> (Listof Field)
;; Update field values from the current field table
(define (update-field-values fields table)
  (for/list ([f (in-list fields)])
    (struct-copy field f
                 [val (ephemeron-value
                       (hash-ref table
                                 (synd:field-handle-desc (field-handle f))
                                 (lambda () (field-val f))))])))

(module+ test
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
          (stx:spawn (stx:assert 'hello))
          (sleep 1/4)
          (define evts (channel->list test-ch))
          (define actor-env? hash?)
          (define env-evt (findf actor-env? evts))
          (check-not-false env-evt)
          (check-match env-evt
                       (hash '(2)
                             (hash '(4)
                                   (facet '(4)
                                          '()
                                          (list (endpoint '(stx:assert 'hello)
                                                          (? srcloc?)))
                                          (== (set))))))))))

  (test-case "update-process-state removes dead facets"
    (define test-facets
      (hash '(1) (make-facet '(1))
            '(2) (make-facet '(2))))
    (define test-state
      (synd:actor-state (void)
                        (hash '(1) (synd:facet '(1) (hash) '() (set) #f #f))
                        (void) (void) (hash) (void)))
    (check-equal? (hash-keys (update-process-state test-facets test-state))
                  '((1))))

  (test-case "update-process-state updates children"
    (define test-facets
      (hash '(1) (make-facet '(1))))
    (define test-state
      (synd:actor-state (void)
                        (hash '(1) (synd:facet '(1) (hash) '() (set '(2) '(3)) #f #f))
                        (void) (void) (hash) (void)))
    (check-equal? (facet-children (hash-ref (update-process-state test-facets test-state) '(1)))
                  (set '(2) '(3))))

  (test-case "update-process-state updates field values"
    (define test-handle (synd:field-handle (synd:field-descriptor 'test 1)))
    (define test-facets
      (hash '(1) (struct-copy facet (make-facet '(1))
                              [fields (list (field test-handle 'old-value (void)))])))
    (define test-state
      (synd:actor-state (void)
                        (hash '(1) (synd:facet '(1) (hash) '() (set) #f #f))
                        (void) (void)
                        (hash (synd:field-handle-desc test-handle)
                              (make-ephemeron (synd:field-handle-desc test-handle) 'new-value))
                        (void)))
    (check-equal? (field-val (car (facet-fields (hash-ref (update-process-state test-facets test-state) '(1)))))
                  'new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON

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
                                         (synd:field-handle (synd:field-descriptor 'test #f))
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
                                             'span 10))))))))
