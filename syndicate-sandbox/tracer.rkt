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
    [('action-interpreted (? patch? p))
     (define who (spacetime-space source))
     (define act (hash-ref (dataspace-actors ds) who))
     (struct-copy dataspace ds
                  [actors (hash-set (dataspace-actors ds)
                                    who
                                    (struct-copy actor act
                                                 [pending-acts (remove-action (actor-pending-actions act) p source)]
                                                 [assertions (apply-patch (actor-assertions act) p)]))])]
    [('action-interpreted (message body))
     (write-event! source sink 'action-interpreted
                   'message
                   (pretty-format body))]
    [('action-interpreted 'quit)
     (hash-remove! names (spacetime-space source))
     (write-event! source sink 'quit)]
    [('event (list cause (? patch? p)))
     (match (spacetime-space sink)
       ['()
        (write-event! source sink 'event
                      'patch
                      (patch->pretty-string p)
                      cause
                      (list (spacetime-space cause)))]
       [(cons _ context-path)
        (write-event! source sink 'event
                      'patch
                      (format-patch '#hash() context-path p)
                      cause
                      (set-map (extract-patch-pids p)
                               (lambda (local-pid) (cons local-pid context-path))))])]
    [('event (list cause (message body)))
     (write-event! source sink 'event
                   'message
                   (pretty-format body)
                   cause
                   (list (spacetime-space cause)))]
    [('event (list _cause #f)) ;; cause will be #f
     (void)]))

#;(struct-copy actor act
               [pending-acts (remove-action/actor (actor-pending-actions act) action source)]
               [assertions (apply-patch (actor-assertions act) p)])

(define (remove-action ds action source)
  (define who (spacetime-space source))
  (define act (hash-ref (dataspace-actors ds) who))
  (struct-copy dataspace ds
               [actors (hash-set (dataspace-actors ds)
                                 who
                                 (remove-action/actor act action source))]))

(define (remove-action/actor act action label)
  (define pending-acts (actor-pending-acts act))
  (define target (findf (lambda (p) (equal? label (pending-origin p)))
                        pending-acts))
  (match target
    [#f
     pending-acts]
    [(pending _ acts)
     (define new-acts (remove action acts))
     (cond
       [(empty? new-acts)
        (remove target pending-acts)]
       [else
        (cons (pending label new-acts)
              (remove target pending-acts))])]
    [else
     pending-acts]))

(module+ test
  ;; Test remove-action/actor
  (test-case "remove-action/actor tests"
    ))
