#lang scheme/base

(require
 scheme/base
 scheme/contract
 scheme/match
 scheme/dict
 syntax/id-table
 "../primitives.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt"))

(define (transform node)
  (unify-ids node (make-immutable-hash '())))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define lookup-global
  (let ([globals (make-hash)])
    (lambda (id)
      (hash-ref globals id (lambda ()
                             (hash-set! globals id id)
                             id)))))

(define (extend-env env ids)
  (foldr (lambda (id hash) (hash-set hash id id)) env ids))

(define (unify-ids node env)
  (match node
    [(s:literal) node]
    [(s:lexical-ref id loc) (s:lexical-ref (hash-ref env id) loc)]
    [(s:global-ref id loc)
     (if (sines-primitive-id? id)
         (error 'unify-ids "primitive ~S used as a value" (module-id-name id))
         (s:global-ref (lookup-global id) loc))]
    [(s:app op args loc)
     (match op
       [(s:global-ref (and (struct module-id _)
                           (? sines-primitive-id?)
                           id))
        (let ([primitive (lookup-sines-primitive id)])
          (s:primapp primitive (list-unify-ids args env) loc))]
       [_ (s:app (unify-ids op env) (list-unify-ids args env) loc)])]
    [(s:begin body loc) (s:begin (list-unify-ids body env) loc)]
    [(s:lambda ids rest-id body loc)
     (let ([new-env (let ([ne (extend-env env ids)])
                      (if rest-id
                          (extend-env ne (list rest-id))
                          ne))])
       (s:lambda ids rest-id (unify-ids body new-env) loc))]
    [(struct dispatch-lambda-stx [loc procs])       (s:dispatch-lambda (list-unify-ids procs env) loc)]
    [(struct if-stx [loc test then else])           (s:if (unify-ids test env) (unify-ids then env) (unify-ids else env) loc)]
    [(struct let-values-stx [loc idss vals body])
     (s:let-values idss (list-unify-ids vals env) (unify-ids body (extend-env env (apply append idss))) loc)]
    [(struct letrec-values-stx [loc idss vals body])
     (let ([new-env (extend-env env (apply append idss))])
       (s:letrec-values idss (list-unify-ids vals new-env) (unify-ids body new-env) loc))]
    [(struct define-values-stx [loc ids value])     (s:define-values (map lookup-global ids) (unify-ids value env) loc)]
    [(struct set!-stx [loc var value])              (s:set! (unify-ids var env) (unify-ids value env) loc)]
    [(struct wcm-stx [loc key value expr])          (s:wcm (unify-ids key env) (unify-ids value env) (unify-ids expr env) loc)]
    [(struct program-stx [loc body])                (s:program (list-unify-ids body env) loc)]
    [_ (error "unify-ids: not matched" node)]))

(define (list-unify-ids nodes env)
  (map (lambda (a) (unify-ids a env)) nodes))
