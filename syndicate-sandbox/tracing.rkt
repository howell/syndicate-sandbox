#lang racket

(provide (all-defined-out))

(require racket/async-channel)

(define-logger syndicate-trace)

(define current-trace-channel (make-parameter (make-async-channel)))
