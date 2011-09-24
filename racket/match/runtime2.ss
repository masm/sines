#lang s-exp "../../base-lang.rkt"

(define (match:error val srcloc)
  (error 'match "no matching clause for " val srcloc))

(define (srcloc source line column position span)
  (list source line column position span))

(provide
 match:error
 srcloc)