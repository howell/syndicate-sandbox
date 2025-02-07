#lang racket

(provide make-trace-integrator
         current-trace-channel
         dataspace->json
         actor->json)

(require syndicate/trace
         syndicate/trie
         syndicate/tset
         syndicate/patch
         (prefix-in synd: syndicate/core)
         racket/async-channel
         json)

(module+ test
  (require rackunit)
  (require syndicate/tset))

;; an Actor is a (actor Name Trie (Listof Event))
(struct actor (name assertions) #:transparent)
(define (new-actor name) (actor name trie-empty))

;; a PendingAction is a (pending SpaceTime (Listof Action))
(struct pending (origin acts) #:transparent)

;; an ActiveActor is a (List ActorPath Event (Option (Listof Action)))
;; represents an actor's current turn state: who, what event, and what actions produced

;; a Dataspace is a
;; (dataspace (Hashof ActorPath Actor)
;;            (Optionof (List ActorPath Event (Optionof (Listof Action))))
;;            (Listof Any)
;;            (Listof PendingAction)
;;            (Optionof Symbol))
(struct dataspace (actors active recent-messages pending-acts last-op) #:transparent)

(define current-trace-channel (make-parameter (make-async-channel)))

(define (make-trace-integrator [ch (current-trace-channel)] #:max-messages [max-msgs 5])
  (define curr-ds (dataspace (hash) #f '() '() #f))
  (define (receive-notification n)
    (define next-ds (limit-msgs (apply-notification curr-ds n) max-msgs))
    (unless (eq? next-ds curr-ds)
      (set! curr-ds next-ds)
      (async-channel-put ch next-ds)))
  receive-notification)

(define (apply-notification ds/pre n)
  (match-define (trace-notification source sink type detail) n)
  (define ds (mark-last-op ds/pre type))
  (match* (type detail)
    [('turn-begin _process)
     (match (dataspace-active ds)
       [(list who evt _) (struct-copy dataspace ds
                                     [active (list who evt #f)])]
       [_ ds])]
    [('turn-end _process)
     (struct-copy dataspace ds
                  [active #f]
                  [pending-acts (if (dataspace-active ds)
                                  (cons (pending (spacetime-space (first (dataspace-active ds))) '())
                                       (dataspace-pending-acts ds))
                                  (dataspace-pending-acts ds))])]
    [('spawn (synd:process name _beh _state))
     (struct-copy dataspace (remove-action ds synd:actor? source)
                  [actors (hash-set (dataspace-actors ds) (spacetime-space sink) (new-actor name))])]
    [('exit exn-or-false)
     (remove-actor ds (spacetime-space sink))]
    [('actions-produced actions)
     (match (dataspace-active ds)
       [(list who evt _)
        (define labeled-actions (label-actions actions))
        (struct-copy dataspace ds
                     [active (list who evt labeled-actions)]
                     [pending-acts (cons (pending sink labeled-actions)
                                       (dataspace-pending-acts ds))])]
       [_ ds])]
    [('action-interpreted (? synd:patch? p))
     (define p* (patch-relabel p (const DEFAULT-LABEL)))
     (define who (spacetime-space source))
     (apply-patch (remove-action ds p* source) who p*)]
    [('action-interpreted (? synd:message? m))
     (enqueue-message (remove-action ds m source) m)]
    [('action-interpreted 'quit)
     (remove-actor (remove-action ds 'quit source) (spacetime-space source))]
    [('event (list _cause #f))
     ds]
    [('event (list _cause evt))
     (define who (spacetime-space sink))
     (struct-copy dataspace ds
                  [active (list who evt #f)])]))

(define (mark-last-op ds type)
  (define label (match type
                  ['turn-begin 'begin-turn]
                  ['turn-end 'end-turn]
                  ['spawn 'spawn-actor]
                  ['exit 'actor-exit]
                  ['event 'dispatch-event]
                  [_ type]))
  (struct-copy dataspace ds [last-op label]))

;; (Listof Action) -> (Listof Action)
;; Relabel the leaves of every patch to simplify equality checking
(define DEFAULT-LABEL (datum-tset #t))
(define (label-actions actions)
  (for/list ([a (in-list actions)])
    (if (patch? a)
        (patch-relabel a (const DEFAULT-LABEL))
        a)))

;; Dataspace (U Action {Action -> Bool} SpaceTime -> Dataspace
(define (remove-action ds action source)
  (define target (findf (lambda (act) (equal? source (pending-origin act)))
                        (dataspace-pending-acts ds)))
  (match target
    [#f ds]
    [(pending _ acts)
     (define finder (if (procedure? action)
                        (lambda (_ other) (action other))
                        (lambda (_ other) (equal? other action))))
     (define other-acts (remove action acts finder))
     (define next-acts
       (cond
         [(empty? other-acts)
          (remove target (dataspace-pending-acts ds))]
         [else
          (cons (pending source other-acts)
                (remove target (dataspace-pending-acts ds)))]))
     (struct-copy dataspace ds
                  [pending-acts next-acts])]))

(module+ test
  (test-case "remove-action"
    ; Test empty dataspace
    (check-equal? (remove-action (dataspace (hash) #f '() '() #f) 'action (spacetime 'source 123))
                  (dataspace (hash) #f '() '() #f)
                  "Empty dataspace should return unchanged")

    ; Test when action not found
    (check-equal? (remove-action
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '() '() 'op)
                   'action
                   (spacetime 'other-source 49))
                  (dataspace (hash 'actor1 (new-actor 'test)) #f '() '() 'op)
                  "Should return unchanged when action not found")

    ; Test removing action
    (check-equal? (remove-action
                   (dataspace
                    (hash 'actor1 (new-actor 'test))
                    #f
                    '()
                    (list (pending (spacetime 'actor1 45) (list 'action)))
                    'op)
                   'action
                   (spacetime 'actor1 45))
                  (dataspace
                   (hash 'actor1 (new-actor 'test))
                   #f
                   '()
                   '()
                   'op)
                  "Should remove action from actor's pending actions")

    ; Test with multiple actions/actors
    (check-equal? (remove-action
                   (dataspace
                    (hash 'actor1 (new-actor 'test1)
                          'actor2 (new-actor 'test2)
                          'actor3 (new-actor 'test3))
                    #f
                    '()
                    (list
                     (pending (spacetime 'actor1 12) (list 'action1))
                     (pending (spacetime 'actor2 18) (list 'abc 'action2 'def))
                     (pending (spacetime 'actor3 43) (list 'action3)))
                    'op)
                   'action2
                   (spacetime 'actor2 18))
                  (dataspace
                   (hash 'actor1 (new-actor 'test1)
                         'actor2 (new-actor 'test2)
                         'actor3 (new-actor 'test3))
                   #f
                   '()
                   (list
                    (pending (spacetime 'actor2 18) (list 'abc 'def))
                    (pending (spacetime 'actor1 12) (list 'action1))
                    (pending (spacetime 'actor3 43) (list 'action3)))
                   'op)
                  "Should only modify the targeted list of actions")))

;; Actor Patch -> Actor
;; Update an actor's assertions by applying the patch
(define (update-actor-assertions act p)
  (struct-copy actor act
               [assertions (synd:apply-patch (actor-assertions act) p)]))

;; Dataspace ActorPath Patch -> Dataspace
;; Update the designated actor's current assertions based on the patch
(define (apply-patch ds who p)
  (update-actor ds who (lambda (act) (update-actor-assertions act p))))

(module+ test
  (test-case "update-actor-assertions"
    ; Test applying empty patch
    (check-equal? (update-actor-assertions (new-actor 'test) (synd:patch trie-empty trie-empty))
                  (new-actor 'test)
                  "Empty patch should not modify actor")

    ; Test applying non-empty patch
    (define test-trie (pattern->trie (datum-tset 'test) 'value))
    (check-equal? (update-actor-assertions
                   (new-actor 'test)
                   (synd:patch test-trie trie-empty))
                  (struct-copy actor (new-actor 'test)
                               [assertions test-trie])
                  "Should update assertions with patch"))

  (test-case "apply-patch"
    ; Test empty dataspace
    (check-equal? (apply-patch (dataspace (hash) #f '() '() 'op) 'actor1 (synd:patch trie-empty trie-empty))
                  (dataspace (hash) #f '() '() 'op)
                  "Empty dataspace should return unchanged")

    ; Test when actor not found
    (check-equal? (apply-patch
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '() '() 'op)
                   'actor2
                   (synd:patch trie-empty trie-empty))
                  (dataspace (hash 'actor1 (new-actor 'test)) #f '() '() 'op)
                  "Should return unchanged when actor not found")

    ; Test applying patch to actor
    (define test-trie (pattern->trie (datum-tset 'test) 'value))
    (check-equal? (apply-patch
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '() '() 'op)
                   'actor1
                   (synd:patch test-trie trie-empty))
                  (dataspace
                   (hash 'actor1 (struct-copy actor (new-actor 'test)
                                              [assertions test-trie]))
                   #f
                   '()
                   '()
                   'op)
                  "Should apply patch to actor's assertions")

    ; Test with multiple actors
    (check-equal? (apply-patch
                   (dataspace
                    (hash 'actor1 (new-actor 'test1)
                          'actor2 (new-actor 'test2)
                          'actor3 (new-actor 'test3))
                    #f
                    '()
                    '()
                    'op)
                   'actor2
                   (synd:patch test-trie trie-empty))
                  (dataspace
                   (hash 'actor1 (new-actor 'test1)
                         'actor2 (struct-copy actor (new-actor 'test2)
                                              [assertions test-trie])
                         'actor3 (new-actor 'test3))
                   #f
                   '()
                   '()
                   'op)
                  "Should only modify the targeted actor"))

  )

;; Dataspace ActorPath {Actor -> Actor} -> Dataspace
;; Updates the dataspace by applying the given function to the designated actor, if present
(define (update-actor ds who f)
  (define act (hash-ref (dataspace-actors ds) who #f))
  (if act
      (struct-copy dataspace ds
                   [actors (hash-set (dataspace-actors ds)
                                     who
                                     (f act))])
      ds))

(module+ test
  (test-case "update-actor"
    ; Test empty dataspace
    (check-equal? (update-actor (dataspace (hash) #f '() '() 'op)
                                'actor1
                                (lambda (act) (struct-copy actor act [name 'new-name])))
                  (dataspace (hash) #f '() '() 'op)
                  "Empty dataspace should return unchanged")

    ; Test when actor not found
    (check-equal? (update-actor
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '() '() 'op)
                   'actor2
                   (lambda (act) (struct-copy actor act [name 'new-name])))
                  (dataspace (hash 'actor1 (new-actor 'test)) #f '() '() 'op)
                  "Should return unchanged when actor not found")

    ; Test updating single actor
    (check-equal? (update-actor
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '() '() 'op)
                   'actor1
                   (lambda (act) (struct-copy actor act [name 'new-name])))
                  (dataspace
                   (hash 'actor1 (struct-copy actor (new-actor 'test)
                                              [name 'new-name]))
                   #f
                   '()
                   '()
                   'op)
                  "Should update the actor with given function")

    ; Test with multiple actors
    (check-equal? (update-actor
                   (dataspace
                    (hash 'actor1 (new-actor 'test1)
                          'actor2 (new-actor 'test2)
                          'actor3 (new-actor 'test3))
                    #f
                    '()
                    '()
                    'op)
                   'actor2
                   (lambda (act) (struct-copy actor act [name 'new-name])))
                  (dataspace
                   (hash 'actor1 (new-actor 'test1)
                         'actor2 (struct-copy actor (new-actor 'test2)
                                              [name 'new-name])
                         'actor3 (new-actor 'test3))
                   #f
                   '()
                   '()
                   'op)
                  "Should only modify the targeted actor")))


;; Dataspace Message -> Dataspace
;; Adds the given message to the dataspace's recent messages
(define (enqueue-message ds m)
  (struct-copy dataspace ds
               [recent-messages (cons m (dataspace-recent-messages ds))]))

;; Dataspace Natural -> Dataspace
;; Drop all but the N most recent messages in the dataspace
(define (limit-msgs ds n)
  (cond
    [(< n (length (dataspace-recent-messages ds)))
     (struct-copy dataspace ds
                  [recent-messages (take (dataspace-recent-messages ds)
                                         n)])]
    [else
     ds]))

(module+ test
  (test-case "enqueue-message"
    ; Test empty dataspace
    (check-equal? (enqueue-message (dataspace (hash) #f '() '() 'op) 'msg1)
                  (dataspace (hash) #f (list 'msg1) '() 'op)
                  "Should add message to empty list")

    ; Test adding to existing messages
    (check-equal? (enqueue-message (dataspace (hash) #f (list 'msg1 'msg2) '() 'op) 'msg3)
                  (dataspace (hash) #f (list 'msg3 'msg1 'msg2) '() 'op)
                  "Should prepend message to existing list"))

  (test-case "limit-msgs"
    ; Test empty dataspace
    (check-equal? (limit-msgs (dataspace (hash) #f '() '() 'op) 5)
                  (dataspace (hash) #f '() '() 'op)
                  "Empty message list should remain empty")

    ; Test when under limit
    (check-equal? (limit-msgs (dataspace (hash) #f (list 'msg1 'msg2) '() 'op) 5)
                  (dataspace (hash) #f (list 'msg1 'msg2) '() 'op)
                  "Under-limit list should remain unchanged")

    ; Test when at limit
    (check-equal? (limit-msgs (dataspace (hash) #f (list 'msg1 'msg2 'msg3) '() 'op) 3)
                  (dataspace (hash) #f (list 'msg1 'msg2 'msg3) '() 'op)
                  "At-limit list should remain unchanged")

    ; Test when over limit
    (check-equal? (limit-msgs (dataspace (hash) #f (list 'msg1 'msg2 'msg3 'msg4 'msg5) '() 'op) 3)
                  (dataspace (hash) #f (list 'msg1 'msg2 'msg3) '() 'op)
                  "Over-limit list should be truncated")))

;; Dataspace ActorPath -> Dataspace
(define (remove-actor ds who)
  (struct-copy dataspace ds
               [actors (hash-remove (dataspace-actors ds) who)]))

(module+ test
  (require syndicate/store
           syndicate/ground)

  (define (run/record boot-acts)
    (define evts/rev '())
    (define (record-evt! evt) (set! evts/rev (cons evt evts/rev)))
    (with-store [(current-trace-procedures (cons record-evt! (current-trace-procedures)))]
      (run-ground boot-acts))
    (reverse evts/rev))

  (define (consume-trace t)
    (for/fold ([ds (dataspace (hash) #f '() '() #f)])
              ([evt (in-list t)])
      (apply-notification ds evt)))

  (require (only-in (submod syndicate/examples/actor/bank-account syndicate-main) activate!))
  (define bank-account-trace (parameterize ([current-output-port (open-output-nowhere)])
                               (run/record (activate!))))

  (test-case "consumes a real trace"
    (check-not-exn (lambda () (consume-trace bank-account-trace))))

  (test-case "final state reflects program execution"
    (define final-ds (consume-trace bank-account-trace))
    (check-equal? (hash-count (dataspace-actors final-ds))
                  2)
    (check-false (dataspace-active final-ds))
    (check-equal? (length (dataspace-recent-messages final-ds))
                  2)
    (check-true (empty? (dataspace-pending-acts final-ds))))

  (test-case "removing actions regression"
    (define the-action (patch (trie '#s(account 0) DEFAULT-LABEL
                                    '#s(observe (deposity 'any)) DEFAULT-LABEL)
                              trie-empty))
    (define the-ds (dataspace (hash)
                              #f
                              '()
                              (list (pending '#s(spacetime (0) 2) (list the-action)))
                              'op))
    (check-equal? (apply-notification the-ds (trace-notification
                                              '#s(spacetime (0) 2)
                                              '#s(spacetime () 21)
                                              'action-interpreted
                                              the-action))
                  (struct-copy dataspace the-ds
                               [pending-acts '()]
                               [last-op 'action-interpreted]
                               ))))

;; Dataspace -> JSExpr
(define (dataspace->json ds)
  (hash 'actors (for/list ([(k v) (in-hash (dataspace-actors ds))])
                  (hash-set (actor->json v) 'id (~a k)))
        'active_actor (match (dataspace-active ds)
                        [#f #f]
                        [(list who evt acts) (hash 'actor (~a who)
                                                 'event (action->json evt)
                                                 'actions (and acts (map action->json acts)))])
        'recent_messages (map action->json (dataspace-recent-messages ds))
        'pending_actions (for/list ([p (in-list (dataspace-pending-acts ds))])
                           (hash 'origin (spacetime->json (pending-origin p))
                                 'actions (map action->json (pending-acts p))))
        'last_op (and (dataspace-last-op ds) (~a (dataspace-last-op ds)))))

;; Actor -> JSExpr
(define (actor->json act)
  (hash 'name (~a (actor-name act))
        'assertions (trie->json (actor-assertions act))))

;; Action -> JSExpr
(define (action->json a)
  (cond
    [(patch? a)
     (patch->json a)]
    [(synd:actor? a)
     (list "spawn" (trie->json (synd:actor-initial-assertions a)))]
    [(synd:message? a)
     (list "message" (~v a))]
    [else
     (~v a)]))

;; Patch -> JSExpr
(define (patch->json p)
  (hash 'added (trie->json (patch-added p))
        'removed (trie->json (patch-removed p))))

;; Trie -> JSExpr
(define (trie->json t)
  (match-define (list (list* _ added) _) (trie->patterns t))
  (map ~v added))

;; SpaceTime -> JSExpr
(define (spacetime->json st)
  (match st
    [#f #f]
    [(spacetime space time)
     (hash 'space (~a space)
           'time time)]))

(module+ test
  (test-case "dataspace->json"
    (define ds (dataspace (hash) #f '() '() #f))
    (check-equal? (dataspace->json ds)
                  (hash 'actors '()
                        'active_actor #f
                        'recent_messages '()
                        'pending_actions '()
                        'last_op #f))

    (define ds2 (dataspace (hash '(1) (new-actor 'test))
                           (list '(1) 'test-evt (list 'act1))
                           (list 'msg1)
                           (list (pending (spacetime '(1) 123)
                                        (list 'act1)))
                           'action-interpreted))
    (check-equal? (dataspace->json ds2)
                  (hash 'actors (list (hash 'id "(1)"
                                            'name "test"
                                            'assertions '()))
                        'active_actor (hash 'actor "(1)"
                                          'event "'test-evt"
                                          'actions '("'act1"))
                        'recent_messages (list "'msg1")
                        'last_op "action-interpreted"
                        'pending_actions (list (hash 'origin (hash 'space "(1)"
                                                                   'time 123)
                                                     'actions (list "'act1"))))))

  (test-case "actor->json"
    (define act (new-actor 'test))
    (check-equal? (actor->json act)
                  (hash 'name "test"
                        'assertions '()))

    (define act2 (struct-copy actor act
                              [assertions (pattern->trie (datum-tset 'test) 'value)]))
    (check-match (actor->json act2)
                 (hash-table ['name "test"]
                             ['assertions '("'value")])))

  (test-case "real trace elements produces legal json"
    (void (for/fold ([ds (dataspace (hash) #f '() '() #f)])
                    ([evt (in-list bank-account-trace)])
            (define v (dataspace->json ds))
            (check-not-exn (lambda () (jsexpr->string v)) (~a v))
            #;(check-true (jsexpr? v) (~a v))
            (apply-notification ds evt))))

  (test-case "regression: failing to properly remove actions"
    (local-require syndicate/drivers/repl
                   (only-in (submod syndicate/drivers/repl syndicate-main) activate!))
    (define repl-trace (parameterize ([current-output-port (open-output-nowhere)])
                         (thread (lambda ()
                                   (sleep 1/4)
                                   (do-quit)))
                         (run/record (activate!))))
    (define final-ds (consume-trace repl-trace))
    (check-match (dataspace-pending-acts final-ds)
                 (list (pending _ (list (? synd:quit-dataspace?)))))))
