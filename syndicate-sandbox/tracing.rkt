#lang racket

(provide (all-defined-out))

(require racket/async-channel)

(define-logger sandbox-trace)

(define current-trace-channel (make-parameter (make-async-channel)))
