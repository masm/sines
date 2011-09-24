#lang s-exp "../base-lang.rkt"

(define (find proc list)
  (let loop ([list list])
    (cond [(null? list) #f]
          [(pair? list)
           (if (proc (car list))
               (car list)
               (loop (cdr list)))]
          [else (error 'find "second argument must be a proper list" list)])))

(define for-all
  (case-lambda
    [(proc list)
     (let loop ([list list])
       (cond [(null? list) #t]
             [(pair? list) (and (proc (car list)) (loop (cdr list)))]
             [else (error 'for-all "second argument must be a proper list" list)]))]
    [(proc list . lists)
     (for-all (lambda (b) b) (apply map proc list lists))]))

(define (filter pred list)
  (let loop ([list list] [acc '()])
    (cond [(null? list)
           (reverse acc)]
          [(pair? list)
           (loop (cdr list) (if (pred (car list))
                                (cons (car list) acc)
                                acc))]
          [else (error 'filter "second argument must be a proper list" list)])))

(define (partition pred list)
  (let loop ([list list]
             [acc1 '()]
             [acc2 '()])
    (cond [(null? list)
           (values (reverse acc1) (reverse acc2))]
          [(pair? list)
           (if (pred (car list))
               (loop (cdr list) (cons (car list) acc1) acc2)
               (loop (cdr list) acc1 (cons (car list) acc2)))]
          [else (error 'partition "second argument must be a proper list" list)])))

(define (remx name proc list)           ; aux
  (let loop ((l list) (acc '()))
    (cond ((null? l)
           (reverse acc))
          ((pair? l)
           (loop (cdr l) (if (proc (car l))
                             acc
                             (cons (car l) acc))))
          (else (error name "second argument must be a proper list" list)))))

(define (remp proc list)
  (remx 'remp proc list))

(define (remq obj list)
  (remx 'remq (lambda (x) (eq? obj x)) list))

(define (remv obj list)
  (remx 'remv (lambda (x) (eqv? obj x)) list))

(define (remove obj list)
  (remx 'remove (lambda (x) (equal? obj x)) list))

  ;; (define-values (remp remq remv remove)
  ;;     (let ((remx (lambda (name proc list)
  ;;                   (let loop ((l list) (acc '()))
  ;;                     (cond ((null? l)
  ;;                            (reverse acc))
  ;;                           ((pair? l)
  ;;                            (loop (cdr list) (if (proc (car l))
  ;;                                                 acc
  ;;                                                 (cons (car l) acc))))
  ;;                           (else (error name "second argument must be a proper list" list)))))))
  ;;       (values (lambda (proc list)     ; remp
  ;;                 (remx 'remp proc list))

  ;;               (lambda (obj list)      ; remq
  ;;                 (remx 'remq (lambda (x) (eq? obj x)) list))

  ;;               (lambda (obj list)      ; remv
  ;;                 (remx 'remv (lambda (x) (eqv? obj x)) list))

  ;;               (lambda (obj list)      ; remove
  ;;                 (remx 'remove (lambda (x) (equal? obj x)) list)))))

(require "../private/list.ss")
(provide (all-from-out "../private/list.ss"))

(define (assx name proc list)           ; aux
  (let loop ((l list))
    (cond ((null? l)
           #f)
          ((and (pair? l) (pair? (car l)))
           (if (proc (caar l))
               (car l)
               (loop (cdr l))))
          (else (error name "second argument must be an association list" list)))))

(define (assp proc list)
  (assx 'assp proc list))

(define (assq obj list)
  (assx 'assq (lambda (x) (eq? obj x)) list))

(define (assv obj list)
  (assx 'assv (lambda (x) (eqv? obj x)) list))

(define (assoc obj list)
  (assx 'assoc (lambda (x) (equal? obj x)) list))

(provide find
         for-all exists
         filter partition
         ;; fold-left
         ;; fold-right
         remp remq remv remove
         memp memq memv member
         assp assq assv assoc
         list*)

(define (fold-left1 proc init list)
  (let loop ([list list]
             [acc init])
    (if (null? list)
        acc
        (loop (cdr list)
              (proc acc (car list))))))

(define (fold-left-many proc init lists)
  (let loop ([lists lists]
             [acc '()])
    (if (exists null? lists)
        acc
        (loop (map cdr lists)
              (apply proc acc (map car lists))))))

(define (fold-left proc init list . lists)
  (if (null? lists)
      (fold-left1 proc init list)
      (fold-left-many proc init (cons list lists))))

(define (fold-right1 proc init list)
  (let loop ([list list])
    (if (null? list)
        init
        (proc (car list) (loop (cdr list))))))

(define fold-right
  (case-lambda
    [(proc init list) (fold-right1 proc init list)]))

(provide
 fold-left
 fold-right)

;;; TOMO

(define (filter-map proc l . ls)
  (if (null? ls)
      (let loop ([l l] [acc '()])
        (if (null? l)
            (reverse acc)
            (let ([v (proc (car l))])
              (loop (cdr l)
                    (if v
                        (cons v acc)
                        acc)))))
      (filter identity (apply map proc l ls))))

(provide filter-map)
