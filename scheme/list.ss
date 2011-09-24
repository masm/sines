#lang s-exp "../base-lang.rkt"

(define (last l)
  (if (null? (cdr l))
      (car l)
      (last (cdr l))))

(define (append-map proc l . ls)
  (apply append (apply map proc l ls)))

(provide last
         append-map)
