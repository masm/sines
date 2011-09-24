#lang s-exp "../base-lang.ss"

(require
 "../rnrs/list-utilities.ss"
 "sdict.ss")

(define-syntax (sset stx)
  (syntax-case stx ()
    [(_ s ...)
     (andmap (lambda (stx) (string? (syntax-e stx)))
             (syntax->list #'(s ...)))
     (with-syntax ([(l ...) #'((s #t) ...)])
       #'(sdict l ...))]))

(define (sset-copy set)
  (list->sset (sset->list set)))

(define (sset-add! set str)
  (sdict-set! set str #t))

(define (sset-remove! set str)
  (sdict-remove! set str))

(define (sset-member? set str)
  (sdict-key? set str))

(define (sset->list set)
  (sdict-keys set))

(define (sset-cardinality set)
  (length (sset->list set)))

(provide sset
         sset-copy
         sset-add!
         sset-remove!
         sset-member?
         sset->list
         sset-cardinality)

;;;

(define (sset-subset? s1 s2)
  (for-all (lambda (k)
             (sset-member? s2 k))
           (sset->list s1)))

(define (sset-equal? s1 s2)
  (and (sset-subset? s1 s2) (sset-subset? s2 s1)))

(define (sset-union s1 s2)
  (let ([set (sset)])
    (for-each (lambda (k) (sset-add! set k))
              (sset->list s1))
    (for-each (lambda (k) (sset-add! set k))
              (sset->list s2))
    set))

(define (sset-intersection s1 s2)
  (let ([set (sset)])
    (for-all (lambda (k)
               (when (sset-member? s2 k)
                 (sset-add! set k)))
             (sset->list s1))
    set))

(define (sset-minus s1 s2)
  (let ([set (sset)])
    (for-all (lambda (k)
               (unless (sset-member? s2 k)
                 (sset-add! set k)))
             (sset->list s1))
    set))

(define (list->sset l)
  (let ([set (sset)])
    (for-each (lambda (k) (sset-add! set k))
              l)
    set))

(define (sset->vector set)
  (list->vector (sset->list set)))

(define (vector->sset vec)
  (let ([set (sdict)])
    (vector-for-each (lambda (k) (sset-add! set k))
                     vec)
    set))

(provide sset-subset?
         sset-equal?
         sset-union
         sset-intersection
         sset-minus
         list->sset
         sset->vector
         vector->sset)
