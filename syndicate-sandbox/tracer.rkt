#lang racket

(provide make-tracer)

(require syndicate/trace
         syndicate/trie
         racket/async-channel
         (prefix-in synd: syndicate/core))

(module+ test
  (require rackunit))

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
    (define next-ds (apply-notification curr-ds n))
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
     (define act (hash-ref (dataspace-actors ds) who))
     (struct-copy dataspace ds
                  [actors (hash-set (dataspace-actors ds)
                                    who
                                    (struct-copy actor act
                                                 [pending-acts (remove-action (actor-pending-acts act) p source)]
                                                 [assertions (synd:apply-patch (actor-assertions act) p)]))])]
    [('action-interpreted (synd:message body))
     #f]
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
  (define who (spacetime-space source))
  (define act (hash-ref (dataspace-actors ds) who #f))
  (if act
      (struct-copy dataspace ds
                   [actors (hash-set (dataspace-actors ds)
                                     who
                                     (remove-action/actor act action source))])
      ds))

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

;; Dataspace ActorPath Patch -> Dataspace
;; update the designated actor's current assertions based on the patch
(define (apply-patch ds who p)
  )
