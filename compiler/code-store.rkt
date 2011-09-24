#lang scheme/base

(require scheme/base
         (for-syntax scheme/base))

(define store '())

(define (add-code syntax)
  (set! store (cons syntax store)))

(define (code-list)
  store)

(provide add-code code-list)
