#lang s-exp "bootstrap-lang.rkt"

(require
 "library.ss")

(define (qq-append a b)
  (if (list? a)
      (append a b)
      (error 'unquote-splicing "proper list" a)))

(provide
 qq-append)
