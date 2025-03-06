#lang info
(define collection 'multi)
(define deps '("base"
               "rackunit"
               "sandbox-lib"
               "syndicate-classic"
               "web-server-lib"
               "threading-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/syndicate-sandbox.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(sam))
(define license '(Apache-2.0 OR MIT))
