#lang racket

(provide (all-defined-out))

(require racket/async-channel)

(define-logger sandbox-trace)

(define current-trace-channel (make-parameter (make-async-channel)))

;; The current-endpoint-notification-handler is an (Optionof {EndpointNotification -> Any})
(define current-endpoint-notification-handler (make-parameter #f))

;; a CombinedNotification is one of
;;   - (notification 'dataspace Dataspace) indicating a step in the dataspace trace
;;   - (notification 'actors ActorEnv) indicating new information about the actors in the dataspace and their facets
(struct notification (type detail) #:transparent)

(define (dataspace-notification? v)
  (and (notification? v)
       (equal? 'dataspace (notification-type v))))

(define (actors-notification? v)
  (and (notification? v)
       (equal? 'actors (notification-type v))))

;; a Dataspace is a
;; (dataspace (Hashof ActorPath Actor)
;;            (Optionof (List ActorPath Event (Optionof (Listof Action))))
;;            (Listof Any)
;;            (Listof PendingAction)
;;            (Optionof Symbol))
(struct dataspace (actors active recent-messages pending-acts last-op) #:transparent)

;; an Actor is a (actor Name Trie (Listof Event))
(struct actor (name assertions) #:transparent)

;; a PendingAction is a (pending SpaceTime (Listof Action))
(struct pending (origin acts) #:transparent)

;; an ActiveActor is a (List ActorPath Event (Option (Listof Action)))
;; represents an actor's current turn state: who, what event, and what actions produced


;; an ActorEnv is a (Hashof PID ActorDetail)

;; an ActorDetail is a (actor-detail (Hashof FID FacetDetail) DataflowGraph)
(struct actor-detail (facets dataflow) #:transparent)

;; a FacetDetail is a (facet FID (Listof Field) (Listof Endpoint) (Setof FID))
(struct facet (id fields eps children) #:transparent)

;; a Field is a (field FieldHandle Any SrcLoc)
(struct field (handle val src) #:transparent)

;; an Endpoint is a (endpoint EID Any SrcLoc)
(struct endpoint (id description src) #:transparent)

;; an EndpointNotification is one of
;;   - (endpoint-notification FID 'endpoint (List EID Any) SrcLoc)
;;   - (endpoint-notification FID 'field FieldHandle SrcLoc)
(struct endpoint-notification (fid desc detail srcloc) #:transparent)
