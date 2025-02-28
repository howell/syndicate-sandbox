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
         "dataspace-trace-integrator.rkt"
         racket/async-channel
         syndicate/trace
         (prefix-in synd: (submod syndicate/actor implementation-details))
         (prefix-in synd: syndicate/core))

(module+ test
  (require rackunit
           "utils.rkt"
           syndicate/store
           syndicate/test/test-dataspace
           (prefix-in stx: "lang.rkt")))

(define (make-facet fid) (facet fid '() '() (set)))

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
    [else
     (values curr-ds curr-facets (cons evt pending-endpoint-evts))]))

;; ActorEnv PID ActorState EndpointNotification -> ActorEnv
(define (associate-endpoint env pid proc-state evt)
  (define fid (endpoint-notification-fid evt))
  (define (update-actor existing-facets)
    (if (hash-has-key? existing-facets fid)
        (hash-update existing-facets
                     fid
                     (lambda (fct) (add-notification fct proc-state evt)))
        existing-facets))
  (hash-update env pid update-actor (hash)))

;; Facet ActorState EndpointNotification -> Facet
(define (add-notification fct proc-state evt)
  (define detail (endpoint-notification-detail evt))
  (define src (endpoint-notification-srcloc evt))
  (case (endpoint-notification-desc evt)
    [(endpoint)
     (add-ep fct detail src)]
    [(field)
     (add-field fct proc-state detail src)]))

(define (add-ep fct desc src)
  (struct-copy facet fct
               [eps (cons (endpoint desc src)
                          (facet-eps fct))]))

(define (add-field fct proc-state handle src)
  (struct-copy facet fct
               [fields (cons (field handle (read-field handle (synd:actor-state-field-table proc-state)) src)
                             (facet-fields fct))]))

(define (apply-pending-evts env pid proc-state evts)
  (for/fold ([env env])
            ([evt (in-list evts)])
    (associate-endpoint env pid proc-state evt)))

;; ActorEnv (Listof EndpointNotification) TraceNotification -> {Values ActorEnv (Listof EndpointNotification)}
(define (check-for-process-update actors pending-endpoint-evts tn)
  (match tn
    [(trace-notification _ who (and ty (or 'turn-begin 'turn-end 'spawn)) proc)
     #:when (synd:actor-state? (synd:process-state proc))
     (define pid (spacetime-space who))
     (define proc-state (synd:process-state proc))

     (define actors* (hash-update actors
                                  pid
                                  (curryr update-process-state proc-state)
                                  (hash)))

     (define this-actor (hash-ref actors* pid))

     ;; Find endpoint notifications that match facet IDs in this actor
     (define-values (matching-evts other-evts)
       (partition (lambda (evt)
                    (and (endpoint-notification? evt)
                         (hash-has-key? this-actor (endpoint-notification-fid evt))))
                  pending-endpoint-evts))

     (values (apply-pending-evts actors* pid proc-state matching-evts)
             other-evts)]
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
                 [val (read-field (field-handle f) table)])))

;; FieldHandle FieldTable -> Any
(define (read-field fh table)
  (ephemeron-value
   (hash-ref table
             (synd:field-handle-desc fh)
             void)))

(module+ test
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
    (check-equal? (facet-children (hash-ref result '(2))) (set)))

  (test-case "bank account example captures field value and both endpoints"
    (struct account (balance) #:prefab)
    (struct deposit (amount) #:prefab)
    (define test-ch (make-async-channel))
    (define on-evt (make-combined-tracer test-ch))
    (parameterize ([current-endpoint-notification-handler on-evt])
      (with-store [(current-trace-procedures (current-trace-procedures (cons on-evt (current-trace-procedures))))]
        (with-test-dataspace []
          (stx:spawn #:name 'banker
                     (stx:field [balance 0])
                     (stx:assert (account (balance)))
                     (stx:on (stx:message (deposit $amount))
                             (balance (+ (balance) amount))))
          (check-true (asserted? (account 0)))
          (define evts (channel->list test-ch))
          (define final-actors
            (for/last ([evt (in-list evts)]
                       #:when (equal? 'actors (notification-type evt)))
              (notification-detail evt)))
          (define final-ds
            (for/last ([evt (in-list evts)]
                       #:when (equal? 'dataspace (notification-type evt)))
              (notification-detail evt)))
          (check-not-false final-actors)
          (check-not-false final-ds)
          (define banker-pid
            (for/first ([(pid act) (in-hash (dataspace-actors final-ds))]
                        #:when (equal? 'banker (actor-name act)))
              pid))
          (check-not-false banker-pid)
          (check-match banker-pid (list (? (curry <= 2))))
          (check-true (hash-has-key? final-actors banker-pid))
          (define banker-detail (hash-ref final-actors banker-pid))
          (check-equal? (hash-count banker-detail) 1)
          (match-define (list banker-root) (hash-values banker-detail))
          (check-match (facet-fields banker-root)
                       (list (field _ 0 _)))
          (check-equal? (length (facet-eps banker-root)) 2)
          (check-match (facet-eps banker-root)
                       (list-no-order (endpoint '(stx:assert (account (balance)))
                                                _)
                                      (endpoint '(stx:on (stx:message (deposit $amount))
                                                         (balance (+ (balance) amount)))
                                                _))))))))

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
