#lang racket/base

(require "syndicate-sandbox/server.rkt"
         racket/cmdline)

(define serve-host (make-parameter "127.0.0.1"))
(define serve-port (make-parameter 4001))

(command-line
 #:once-each
 [("-l") "listen on all IP addresses"
         (serve-host #f)]
 [("-p" "--port") p "specify the port of the server"
                  (serve-port (string->number p))]
 [("--phx-host") ph "Specify the hostname of the phoenix server"
                 (phx-host ph)]
 [("--phx-port") pp "Specify the port number for the phoenix server"
                 (phx-port (string->number pp))]
 #:args ()
 (run-server #:port (serve-port)
             #:host (serve-host)))
