#lang s-exp "bootstrap-lang.rkt"

(require
 "bootstrap.ss"
 "bootstrap2.ss"
 (only-in racket/base =>))

(define-syntax cond
  (lambda (stx)
    (syntax-case stx (else =>)
      [(_)
       #'(void)]
      [(_ [else body ...])
       #'(let () body ...)]
      [(_ [else . _] . _)
       (error 'cond "else clause must be last" stx)]
      [(_ [pred] clause ...)
       #'(let ([var pred])
           (if var var (cond clause ...)))]
      [(_ [pred => proc] clause ...)
       #'(let ([var pred])
           (if var (proc var) (cond clause ...)))]
      [(_ [pred body ...] clause ...)
       #'(if pred (let () body ...) (cond clause ...))])))

(provide
 cond =>)
