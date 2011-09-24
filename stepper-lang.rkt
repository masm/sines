#lang s-exp "sines-lang.rkt"

(require
 "spider-monkey.ss"
 "racket/match.ss")

#;
(define-for-syntax (Q stx)
  (syntax-case Q))

(define-syntax (_#%app stx)
  (syntax-case stx ()
    [(_ op arg ...)
     (syntax-case (local-expand (syntax/loc stx op)
                                'expression '())
         (#%datum)
       [(#%datum . x)
        #'(with-continuation-mark 'debug (list 'appA op)
         (#%app op . args))]
       [op
        #'(with-continuation-mark 'debug (list 'appB 'op)
            ((lambda (f)
               (with-continuation-mark 'debug (list 'appA 'arg ...)
                 (#%app f arg ...)))
             op))])]))

(define-syntax (_if stx)
  (syntax-case stx ()
    [(_ test then else)
     #'(with-continuation-mark 'debug (list 'if 'then 'else)
         (if test then else))]))

#;
(define-syntax (_#%datum stx)
  (syntax-case stx ()
    [(_ . x)
     #'(begin
         (display 'x)
         (#%datum . x))]))

(define (breakpoint-fn x)
  (let ([marks (reverse
                (continuation-mark-set->list
                 (current-continuation-marks)
                 'debug))])
    (for-each display marks)
    (display "")
    (for-each display (reconstruct marks)))
  x)

(define-syntax-rule (breakpoint x)
  (with-continuation-mark 'debug 'break
    (breakpoint-fn x)))

(define (reconstruct x)
  (match x
    [(cons (list 'appA n) m)
     `(,(reconstruct m) ,n)]
    [(cons (list 'appB v) m)
     `(,v ,(reconstruct m))]
    [(cons (list 'if n l) m)
     `(if ,(reconstruct m) ,n ,l)]
    [(cons 'break '())
     '()]))

(provide (except-out (all-from-out "sines-lang.rkt")
                     #%app #;#%datum if)
         (rename-out (_#%app #%app)
                     #;(_#%datum #%datum)
                     (_if if))
         breakpoint)
