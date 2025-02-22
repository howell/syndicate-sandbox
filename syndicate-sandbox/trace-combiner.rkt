#lang racket

(provide make-combined-tracer
         (struct-out notification)
         notification->json
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
         (prefix-in synd: syndicate/core))

(module+ test
  (require rackunit
           syndicate/store
           syndicate/test/test-dataspace
           (prefix-in stx: "lang.rkt")))

;; a CombinedNotification is one of
;;   - (notification 'dataspace Dataspace) indicating a step in the dataspace trace
;;   - (notification 'actors ActorEnv) indicating new information about the actors in the dataspace and their facets
(struct notification (type detail) #:transparent)

;; an ActorEnv is a (Hashof PID ActorDetail)
;; an ActorDetail is a (Hashof FID FacetDetail)

;; a FacetDetail is a (facet FID (Listof Field) (Listof Endpoint) (Setof FID))
(struct facet (id fields eps children) #:transparent)
(define (make-facet fid) (facet fid '() '() (set)))

;; a Field is a (field FieldHandle Any SrcLoc)
(struct field (handle val src) #:transparent)

;; an Endpoint is a (endpoint Any SrcLoc)
(struct endpoint (description src) #:transparent)

;; a TraceEvent is a TraceNotification or an EndpointNotification

;; (Channelof CombinedNotification) -> TraceEventHandler
(define (make-combined-tracer [ch (current-trace-channel)])
  (define curr-ds (dataspace (hash) #f '() '() #f))
  (define curr-actors (hash))
  ;; because the actor's behavior and state are initialized BEFORE the spawn trace event, we need to keep these around
  (define pending-endpoint-evts '())
  (define (on-event evt)
    (define-values (next-ds next-actors next-pending-evts)
      (receive-update curr-ds curr-actors pending-endpoint-evts evt))
    (set! pending-endpoint-evts next-pending-evts)
    (unless (equal? next-ds curr-ds)
      (async-channel-put ch (notification 'dataspace next-ds))
      (set! curr-ds next-ds))
    (unless (equal? next-actors curr-actors)
      (async-channel-put ch (notification 'actors next-actors))
      (set! curr-actors next-actors)))
  on-event)

;; Dataspace ActorEnv TraceEvent -> {Values Dataspace ActorEnv}
(define (receive-update curr-ds curr-facets pending-endpoint-evts evt)
  (cond
    [(trace-notification? evt)
     (define-values (facets* pending*) (check-for-process-update curr-facets pending-endpoint-evts evt))
     (values (apply-notification curr-ds evt)
             facets*
             pending*)]
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
               [fields (cons (field handle (void) src)
                             (facet-fields fct))]))

(define (apply-pending-evts env pid evts)
  (for/fold ([env env])
            ([evt (in-list evts)])
    (associate-endpoint env pid evt)))

;; ActorEnv (Listof EndpointNotification) TraceNotification -> {Values ActorEnv (Listof EndpointNotification)}
(define (check-for-process-update actors pending-endpoint-evts tn)
  (match tn
    [(trace-notification _ who (and ty (or 'turn-begin 'turn-end 'spawn)) proc)
     #:when (synd:actor-state? (synd:process-state proc))
     (define pid (spacetime-space who))
     (define-values (actors* evts*)
       (if (equal? ty 'spawn)
           (values (apply-pending-evts actors pid pending-endpoint-evts) '())
           (values actors pending-endpoint-evts)))
     (define updated-actors (hash-update actors*
                                         pid
                                         (curryr update-process-state (synd:process-state proc))
                                         (hash)))
     (values updated-actors evts*)]
    [(trace-notification _ who 'exit _)
     (values (hash-remove actors (spacetime-space who))
             pending-endpoint-evts)]
    [_
     (values actors pending-endpoint-evts)]))

;; ActorDetail ActorState -> ActorDetail
;; Update the information associated with an actor's facets based on the observed state of the process
(define (update-process-state facets as)
  (define live-facets (synd:actor-state-facets as))
  (define field-table (synd:actor-state-field-table as))

  (define facets-with-new
    (for/hash ([(fid live-facet) (in-hash live-facets)])
      (define existing-info (hash-ref facets fid (lambda () (make-facet fid))))
      (define with-children (struct-copy facet existing-info
                                         [children (synd:facet-children live-facet)]))
      (values fid
              with-children)))

  ;; Update all facets with current field values
  (for/hash ([(fid fct) (in-hash facets-with-new)])
    (values fid
            (struct-copy facet fct
                         [fields (update-field-values (facet-fields fct)
                                                      field-table)]))))

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
          (define actor-evt? (lambda (n) (equal? 'actors (notification-type n))))
          (define env-evt (findf actor-evt? evts))
          (check-not-false env-evt)
          (check-match (notification-detail env-evt)
                       (hash '(2)
                             (hash '(4)
                                   (facet '(4)
                                          '()
                                          (list (endpoint '(stx:assert 'hello)
                                                          (? srcloc?)))
                                          (== (set))))
                             #:open))))))

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
                  'new-value))

  (test-case "update-process-state adds new facets from actor state"
    (define test-facets (hash))
    (define test-state
      (synd:actor-state (void)
                        (hash '(1) (synd:facet '(1) (hash) '() (set '(2) '(3)) #f #f)
                              '(2) (synd:facet '(2) (hash) '() (set) #f #f))
                        (void) (void) (hash) (void)))
    (define result (update-process-state test-facets test-state))
    (check-equal? (hash-keys result) '((1) (2)))
    (check-equal? (facet-children (hash-ref result '(1))) (set '(2) '(3)))
    (check-equal? (facet-children (hash-ref result '(2))) (set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON

;; CombinedNotification -> JSExpr
(define (notification->json n)
  (match n
    [(notification 'dataspace ds)
     (hash 'type "dataspace"
           'detail (dataspace->json ds))]
    [(notification 'actors env)
     (hash 'type "actors"
           'detail (actor-env->json env))]))

;; ActorEnv -> JSExpr
(define (actor-env->json env)
  (for/list ([(pid facets) (in-hash env)])
    (hash 'actor_id (~a pid)
          'facets (facets->json facets))))

;; (Hashof FID FacetDetail) -> JSExpr
(define (facets->json facets)
  (for/list ([(fid detail) (in-hash facets)])
    (hash 'facet_id (~a fid)
          'detail (facet->json detail))))

;; FacetDetail -> JSExpr
(define (facet->json f)
  (hash 'id (~a (facet-id f))
        'fields (map field->json (facet-fields f))
        'endpoints (map endpoint->json (facet-eps f))
        'children (set-map (facet-children f) ~a)))

;; Field -> JSExpr
(define (field->json f)
  (hash 'name (~a (synd:field-descriptor-name (synd:field-handle-desc (field-handle f))))
        'value (~v (field-val f))
        'src (srcloc->json (field-src f))))

;; Endpoint -> JSExpr
(define (endpoint->json e)
  (hash 'description (~a (endpoint-description e))
        'src (srcloc->json (endpoint-src e))))

;; SrcLoc -> JSExpr
(define (srcloc->json loc)
  (hash 'source (~a (srcloc-source loc))
        'line (srcloc-line loc)
        'column (srcloc-column loc)
        'position (srcloc-position loc)
        'span (srcloc-span loc)))

(module+ test
  (define sample-srcloc (srcloc "test.rkt" 1 5 50 10))

  (test-case "srcloc->json converts source location"
    (check-equal? (srcloc->json sample-srcloc)
                 (hash 'source "test.rkt"
                       'line 1
                       'column 5
                       'position 50
                       'span 10)))

  (test-case "endpoint->json converts endpoint"
    (define test-endpoint (endpoint '(assert 'hello) sample-srcloc))
    (check-equal? (endpoint->json test-endpoint)
                 (hash 'description "(assert (quote hello))"
                       'src (srcloc->json sample-srcloc))))

  (test-case "field->json converts field"
    (define test-handle (synd:field-handle (synd:field-descriptor 'test 1)))
    (define test-field (field test-handle 'test-val sample-srcloc))
    (check-equal? (field->json test-field)
                 (hash 'name "test"
                       'value "'test-val"
                       'src (srcloc->json sample-srcloc))))

  (test-case "facet->json converts facet"
    (define test-handle (synd:field-handle (synd:field-descriptor 'test 1)))
    (define test-field (field test-handle 'test-val sample-srcloc))
    (define test-endpoint (endpoint 'test-desc sample-srcloc))
    (define test-facet
      (facet '(1)
             (list test-field)
             (list test-endpoint)
             (set '(2) '(3))))

    (check-equal? (facet->json test-facet)
                 (hash 'id "(1)"
                       'fields (list (field->json test-field))
                       'endpoints (list (endpoint->json test-endpoint))
                       'children '("(3)" "(2)")))))
