#lang info
(define collection "syndicate-sandbox")
(define deps '("base"
               "rackunit"
               "syndicate-classic"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/syndicate-sandbox.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(sam))
(define license '(Apache-2.0 OR MIT))
