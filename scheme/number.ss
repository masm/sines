#lang s-exp "../base-lang.rkt"

(require
 "../es/bridge.ss")

(define-invoker (_random) "random" (es-global "Math"))

(define random
  (case-lambda
    [() (_random)]
    [(i) (floor (* i (_random)))]))

(provide random)