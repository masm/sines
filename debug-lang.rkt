#lang s-exp "sines-lang.rkt"

(require
 "spider-monkey.ss")

(define-syntax (_#%app stx)
  (syntax-case stx ()
    [(_ op arg ...)
     (with-syntax ([(tmp ...) (generate-temporaries #'(arg ...))])
       #'(let ([tmp arg] ...)
           (with-continuation-mark 'debug (list 'op tmp ...)
             (#%app op tmp ...))))]))

(define (breakpoint x)
  (let ([marks (reverse
                (continuation-mark-set->list
                 (current-continuation-marks)
                 'debug))])
    (for-each display marks)
    (display ""))
  x)

(provide (except-out (all-from-out "sines-lang.rkt")
                     #%app
                     ;; #%datum
                     if)
         (rename-out (_#%app #%app)
                     #;(_#%datum #%datum)
                     (_if if))
         breakpoint)
