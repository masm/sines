#lang s-exp "bootstrap-lang.rkt"

(require
 "library.ss"
 "cond.ss")

(define (memx name proc list)           ; aux
  (let loop ((l list))
    (cond [(null? l)
           #f]
          [(pair? l)
           (if (proc (car l))
               l
               (loop (cdr l)))]
          [else (error name "second argument must be a proper list" list)])))

(define (memp proc list)
  (memx 'memp proc list))

(define (memq obj list)
  (memx 'memq (lambda (x) (eq? obj x)) list))

(define (memv obj list)
  (memx 'memv (lambda (x) (eqv? obj x)) list))

(define (member obj list)
  (memx 'member (lambda (x) (equal? obj x)) list))

(provide
 memp memq memv member)

(define (list* obj . objs)
  (let loop ([obj obj] [l objs])
    (if (null? l)
        obj
        (cons obj (loop (car l) (cdr l))))))

(provide
 list*)