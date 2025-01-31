#lang racket

(provide make-tracer)

(require syndicate/trace
         syndicate/trie
         racket/async-channel
         (prefix-in synd: syndicate/core))

(module+ test
  (require rackunit)
  (require syndicate/tset))

;; an Actor is a (actor ? Trie (Listof Event) (Listof PendingAction))
(struct actor (name assertions pending-evts pending-acts) #:transparent)
(define (new-actor name) (actor name trie-empty '() '()))

;; a PendingAction is a (pending SpaceTime (Listof Action))
(struct pending (origin acts) #:transparent)

;; a Dataspace is a (dataspace (Hashof ActorPath Actor) (Optionof ActorPath) (Listof Any))
(struct dataspace (actors active-actor recent-messages) #:transparent)

(define (make-tracer ch #:max-messages [max-msgs 5])
  (define curr-ds (dataspace (hash) #f '()))
  (define (receive-notification n)
    (define next-ds (limit-msgs (apply-notification curr-ds n) 5))
    (set! curr-ds next-ds)
    (async-channel-put ch next-ds))
  receive-notification)

(define (apply-notification ds n)
  (match-define (trace-notification source sink type detail) n)
  (match* (type detail)
    [('turn-begin _process)
     (struct-copy dataspace ds
                  [active-actor sink])]
    [('turn-end _process)
     (struct-copy dataspace ds
                  [active-actor #f])]
    [('spawn (synd:process name _beh _state))
     (struct-copy dataspace ds
                  [actors (hash-set (dataspace-actors ds) (spacetime-space sink) (new-actor name))])]
    [('exit exn-or-false)
     (struct-copy dataspace ds
                  [actors (hash-remove (dataspace-actors ds) (spacetime-space sink))])]
    [('actions-produced actions)
     (cond
       [(null? actions)
        ds]
       [else
        (define origin (spacetime-space sink))
        (struct-copy dataspace ds
                     [actors (hash-update (dataspace-actors ds)
                                          origin
                                          (lambda (act)
                                            (struct-copy actor act
                                                         [pending-acts (cons (pending sink actions)
                                                                             (actor-pending-acts act))])))])])]
    [('action-interpreted (? synd:patch? p))
     (define who (spacetime-space source))
     (update-actor ds who (lambda (act) (update-actor-assertions (remove-action/actor act p source)
                                                                 p)))]
    [('action-interpreted (? synd:message? m))
     (define who (spacetime-space source))
     (enqueue-message (update-actor ds who (lambda (act) (remove-action/actor act m source)))
                      m)]
    [('action-interpreted 'quit)
     #f]
    [('event (list cause (? synd:patch? p)))
     (match (spacetime-space sink)
       ['()
        #f]
       [(cons _ context-path)
        #f])]
    [('event (list cause (synd:message body)))
     #f]
    [('event (list _cause #f)) ;; cause will be #f
     (void)]))

#;(struct-copy actor act
               [pending-acts (remove-action/actor (actor-pending-actions act) action source)]
               [assertions (apply-patch (actor-assertions act) p)])

(define (remove-action ds action source)
  (update-actor ds
                (spacetime-space source)
                (lambda (act) (remove-action/actor act action source))))

(define (remove-action/actor act action label)
  (define pending-acts (actor-pending-acts act))
  (define target (findf (lambda (p) (equal? label (pending-origin p)))
                        pending-acts))
  (match target
    [#f
     act]
    [(pending _ acts)
     (define other-acts (remove action acts))
     (define next-acts
       (cond
         [(empty? other-acts)
          (remove target pending-acts)]
         [else
          (cons (pending label other-acts)
                (remove target pending-acts))]))
     (struct-copy actor act
                  [pending-acts next-acts])]))

(module+ test
  (test-case "remove-action"
    ; Test empty dataspace
    (check-equal? (remove-action (dataspace (hash) #f '()) 'action (spacetime 'source 123))
                  (dataspace (hash) #f '())
                  "Empty dataspace should return unchanged")

    ; Test when actor not found
    (check-equal? (remove-action
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '())
                   'action
                   (spacetime 'other-source 49))
                  (dataspace (hash 'actor1 (new-actor 'test)) #f '())
                  "Should return unchanged when actor not found")

    ; Test removing action from actor
    (check-equal? (remove-action
                   (dataspace
                    (hash 'actor1
                          (struct-copy actor (new-actor 'test)
                                     [pending-acts (list (pending (spacetime 'actor1 45) (list 'action)))]))
                    #f
                    '())
                   'action
                   (spacetime 'actor1 45))
                  (dataspace
                   (hash 'actor1
                         (struct-copy actor (new-actor 'test)
                                    [pending-acts '()]))
                   #f
                   '())
                  "Should remove action from actor's pending actions")

    ; Test with multiple actors
    (check-equal? (remove-action
                   (dataspace
                    (hash 'actor1 (new-actor 'test1)
                          'actor2 (struct-copy actor (new-actor 'test2)
                                             [pending-acts (list (pending (spacetime 'actor2 71) (list 'action)))])
                          'actor3 (new-actor 'test3))
                    #f
                    '())
                   'action
                   (spacetime 'actor2 71))
                  (dataspace
                   (hash 'actor1 (new-actor 'test1)
                         'actor2 (struct-copy actor (new-actor 'test2)
                                            [pending-acts '()])
                         'actor3 (new-actor 'test3))
                   #f
                   '())
                  "Should only modify the targeted actor"))

  ;; Test remove-action/actor
  (test-case "remove-action/actor tests"
    ; Test empty pending actions list
    (check-equal? (remove-action/actor (new-actor 'test) 'action 'label)
                 (new-actor 'test)
                 "Empty list should return unchanged actor")

    ; Test when action not found
    (check-equal? (remove-action/actor
                   (struct-copy actor (new-actor 'test)
                               [pending-acts (list (pending 'other-label (list 'action1 'action2)))])
                   'action
                   'label)
                 (struct-copy actor (new-actor 'test)
                             [pending-acts (list (pending 'other-label (list 'action1 'action2)))])
                 "Should not modify actor when label not found")

    ; Test removing only action
    (check-equal? (remove-action/actor
                   (struct-copy actor (new-actor 'test)
                               [pending-acts (list (pending 'label (list 'action)))])
                   'action
                   'label)
                 (struct-copy actor (new-actor 'test)
                             [pending-acts '()])
                 "Should remove pending entry when last action removed")

    ; Test removing one of multiple actions
    (check-equal? (remove-action/actor
                   (struct-copy actor (new-actor 'test)
                               [pending-acts (list (pending 'label (list 'action1 'action2)))])
                   'action1
                   'label)
                 (struct-copy actor (new-actor 'test)
                             [pending-acts (list (pending 'label (list 'action2)))])
                 "Should keep pending entry with remaining actions")

    ; Test with multiple pending entries
    (check-equal? (remove-action/actor
                   (struct-copy actor (new-actor 'test)
                               [pending-acts (list
                                            (pending 'label1 (list 'action1))
                                            (pending 'label2 (list 'action2))
                                            (pending 'label3 (list 'action3)))])
                   'action2
                   'label2)
                 (struct-copy actor (new-actor 'test)
                             [pending-acts (list
                                          (pending 'label1 (list 'action1))
                                          (pending 'label3 (list 'action3)))])
                 "Should only remove matching pending entry")))

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
    (check-equal? (apply-patch (dataspace (hash) #f '()) 'actor1 (synd:patch trie-empty trie-empty))
                  (dataspace (hash) #f '())
                  "Empty dataspace should return unchanged")

    ; Test when actor not found
    (check-equal? (apply-patch
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '())
                   'actor2
                   (synd:patch trie-empty trie-empty))
                  (dataspace (hash 'actor1 (new-actor 'test)) #f '())
                  "Should return unchanged when actor not found")

    ; Test applying patch to actor
    (define test-trie (pattern->trie (datum-tset 'test) 'value))
    (check-equal? (apply-patch
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '())
                   'actor1
                   (synd:patch test-trie trie-empty))
                  (dataspace
                   (hash 'actor1 (struct-copy actor (new-actor 'test)
                                            [assertions test-trie]))
                   #f
                   '())
                  "Should apply patch to actor's assertions")

    ; Test with multiple actors
    (check-equal? (apply-patch
                   (dataspace
                    (hash 'actor1 (new-actor 'test1)
                          'actor2 (new-actor 'test2)
                          'actor3 (new-actor 'test3))
                    #f
                    '())
                   'actor2
                   (synd:patch test-trie trie-empty))
                  (dataspace
                   (hash 'actor1 (new-actor 'test1)
                         'actor2 (struct-copy actor (new-actor 'test2)
                                            [assertions test-trie])
                         'actor3 (new-actor 'test3))
                   #f
                   '())
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
    (check-equal? (update-actor (dataspace (hash) #f '())
                               'actor1
                               (lambda (act) (struct-copy actor act [name 'new-name])))
                  (dataspace (hash) #f '())
                  "Empty dataspace should return unchanged")

    ; Test when actor not found
    (check-equal? (update-actor
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '())
                   'actor2
                   (lambda (act) (struct-copy actor act [name 'new-name])))
                  (dataspace (hash 'actor1 (new-actor 'test)) #f '())
                  "Should return unchanged when actor not found")

    ; Test updating single actor
    (check-equal? (update-actor
                   (dataspace (hash 'actor1 (new-actor 'test)) #f '())
                   'actor1
                   (lambda (act) (struct-copy actor act [name 'new-name])))
                  (dataspace
                   (hash 'actor1 (struct-copy actor (new-actor 'test)
                                            [name 'new-name]))
                   #f
                   '())
                  "Should update the actor with given function")

    ; Test with multiple actors
    (check-equal? (update-actor
                   (dataspace
                    (hash 'actor1 (new-actor 'test1)
                          'actor2 (new-actor 'test2)
                          'actor3 (new-actor 'test3))
                    #f
                    '())
                   'actor2
                   (lambda (act) (struct-copy actor act [name 'new-name])))
                  (dataspace
                   (hash 'actor1 (new-actor 'test1)
                         'actor2 (struct-copy actor (new-actor 'test2)
                                            [name 'new-name])
                         'actor3 (new-actor 'test3))
                   #f
                   '())
                  "Should only modify the targeted actor")))


;; Dataspace Message -> Dataspace
;; Adds the given message to the dataspace's recent messages
(define (enqueue-message ds m)
  (struct-copy dataspace ds
               [recent-messages (cons m (dataspace-recent-messages ds))]))

;; Dataspace Natural -> Dataspace
;; Drop all but the N most recent messages in the dataspace
(define (limit-msgs ds n)
  (struct-copy dataspace ds
               [recent-messages (take (dataspace-recent-messages ds)
                                    (min n (length (dataspace-recent-messages ds))))]))

(module+ test
  (test-case "enqueue-message"
    ; Test empty dataspace
    (check-equal? (enqueue-message (dataspace (hash) #f '()) 'msg1)
                  (dataspace (hash) #f (list 'msg1))
                  "Should add message to empty list")
    
    ; Test adding to existing messages
    (check-equal? (enqueue-message (dataspace (hash) #f (list 'msg1 'msg2)) 'msg3)
                  (dataspace (hash) #f (list 'msg3 'msg1 'msg2))
                  "Should prepend message to existing list"))
  
  (test-case "limit-msgs"
    ; Test empty dataspace
    (check-equal? (limit-msgs (dataspace (hash) #f '()) 5)
                  (dataspace (hash) #f '())
                  "Empty message list should remain empty")
    
    ; Test when under limit
    (check-equal? (limit-msgs (dataspace (hash) #f (list 'msg1 'msg2)) 5)
                  (dataspace (hash) #f (list 'msg1 'msg2))
                  "Under-limit list should remain unchanged")
    
    ; Test when at limit
    (check-equal? (limit-msgs (dataspace (hash) #f (list 'msg1 'msg2 'msg3)) 3)
                  (dataspace (hash) #f (list 'msg1 'msg2 'msg3))
                  "At-limit list should remain unchanged")
    
    ; Test when over limit
    (check-equal? (limit-msgs (dataspace (hash) #f (list 'msg1 'msg2 'msg3 'msg4 'msg5)) 3)
                  (dataspace (hash) #f (list 'msg1 'msg2 'msg3))
                  "Over-limit list should be truncated")))
