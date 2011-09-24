#lang s-exp "../base-lang.ss"

(require
 "../es/primitives.ss"
 "../rnrs/list-utilities.ss")

(define-syntax (sdict stx)
  (syntax-case stx ()
    [(_ (a b) ...)
     (andmap (lambda (stx) (string? (syntax-e stx)))
             (syntax->list #'(a ...)))
     (with-syntax ([((x ...) ...) #'((a b) ...)])
       #'(es-object x ... ...))]))

(define (sdict-copy dict)
  (let ([d (sdict)])
    (for-each (lambda (k) (sdict-set! d k (sdict-ref dict k)))
              (sdict-keys dict))
    d))

(define (sdict-keys dict)
  (es-object-properties dict))

(define (sdict-key? dict str)
  (es-object-has-own-property? dict str))

(define (sdict-ref dict str)
  (es-property dict str))

(define (sdict-set! dict str value)
  (es-property-set! dict str value))

(define (sdict-remove! dict str)
  (es-delete dict str))

(define (sdict-empty? dict)
  (null? (sdict-keys dict)))

(provide sdict
         sdict-copy
         sdict-keys
         sdict-key?
         sdict-ref
         sdict-set!
         sdict-remove!
         sdict-empty?)

;;;

(define (sdict-intersection d1 d2 [equal? equal?])
  (let ([d (sdict)])
    (for-each (lambda (k)
                (let ([v (sdict-ref d1 k)])
                  (when (and (sdict-key? d2 k)
                             (equal? v (sdict-ref d2 k)))
                    (sdict-set! d k v))))
              (sdict-keys d1))
    d))

(define (sdict-merge! d1 d2 [equal? equal?])
  (for-each (lambda (k) (sdict-set! d1 k (sdict-ref d2 k)))
            (sdict-keys d2))
  d1)

(define (sdict-equal? d1 d2 [equal? equal?])
  (and (for-all (lambda (k)
                  (and (sdict-key? d2 k)
                       (equal? (sdict-ref d1 k) (sdict-ref d2 k))))
                (sdict-keys d1))
       (for-all (lambda (k)
                  (sdict-key? d1 k))
                (sdict-keys d2))))

(define (sdict-minus d1 d2)
  (let ([d (sdict)])
    (for-all (lambda (k)
               (let ([v (sdict-ref d1 k)])
                 (unless (and (sdict-key? d2 k)
                              (equal? v (sdict-ref d2 k)))
                   (sdict-set! d k v))))
             (sdict-keys d1))
    d))

(provide sdict-intersection
         sdict-equal?
         sdict-merge!
         sdict-minus)

;;;

(define (sdict-extend sdict k v)
  (let ([d (sdict-copy sdict)])
    (sdict-set! d k v)
    d))

(provide sdict-extend)

;;;

(define (list->sdict-by-getter lst getter)
  (let ([dict (sdict)])
    (for-each (lambda (d) (sdict-set! dict (getter d) d))
              lst)
    dict))

(define (list->sdict-by-key lst pname)
  (list->sdict-by-getter lst (lambda (d) (sdict-ref d pname))))

(define (vector->sdict-by-getter vec getter)
  (let ([dict (sdict)])
    (vector-for-each (lambda (d) (sdict-set! dict (getter d) d))
                     vec)
    dict))

(define (vector->sdict-by-key vec pname)
  (vector->sdict-by-getter vec (lambda (d) (sdict-ref d pname))))

(provide
 list->sdict-by-getter
 list->sdict-by-key
 vector->sdict-by-getter
 vector->sdict-by-key)

;;;

(define (sdict-copy/map proc dict)
  (let ([new-dict (sdict)])
    (for-each (lambda (k)
                (sdict-set! new-dict k (proc (sdict-ref dict k))))
              (sdict-keys dict))
    new-dict))

(provide sdict-copy/map)