#lang racket

(provide channel->list)

(require racket/async-channel)

(define (channel->list ch)
  (define r (async-channel-try-get ch))
  (if r
      (cons r (channel->list ch))
      '()))
