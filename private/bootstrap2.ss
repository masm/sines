#lang s-exp "bootstrap-lang.rkt"

(require
 "../compiler/primitives.rkt"
 "../compiler/primitives2.rkt")

(define (list . xs)
  xs)

(provide
 list)

(define (void . args)
  (%%void))

(provide
 void)