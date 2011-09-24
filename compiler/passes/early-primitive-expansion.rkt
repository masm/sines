#lang racket/base

(require
 racket/base
 racket/contract
 racket/match
 racket/dict
 syntax/id-table
 "../primitives.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt"))

;; This expands primitives es-property-name! and es-property-name

;; We do two passes for the same reason that the two forms cannot be a macro: we must first
;; expand the es-property-name! forms so that the names that they define can be used by
;; es-property-name in the second pass

(define (transform node)
  (pass2 (pass1 node)))

(define (pass1 node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
    [(s:primapp primitive args loc)
     (if (eq? primitive es-property-name!)
         (apply (sines-primitive-transformer primitive) loc args)
         (s:primapp primitive (map pass1 args) loc))]
    [(s:app op args loc)                  (s:app (pass1 op) (map pass1 args) loc)]
    [(s:begin body loc)                   (s:begin (map pass1 body) loc)]
    [(s:lambda ids rest-id body loc)      (s:lambda ids rest-id (pass1 body) loc)]
    [(s:dispatch-lambda procs loc)        (s:dispatch-lambda (map pass1 procs) loc)]
    [(s:if test then else loc)            (s:if (pass1 test) (pass1 then) (pass1 else) loc)]
    [(s:let-values idss vals body loc)    (s:let-values idss (map pass1 vals) (pass1 body) loc)]
    [(s:letrec-values idss vals body loc) (s:letrec-values idss (map pass1 vals) (pass1 body) loc)]
    [(s:define-values ids value loc)      (s:define-values ids (pass1 value) loc)]
    [(s:set! var value loc)               (s:set! (pass1 var) (pass1 value) loc)]
    [(s:wcm key value expr loc)           (s:wcm (pass1 key) (pass1 value) (pass1 expr) loc)]
    [(s:program body loc)                 (s:program (map pass1 body) loc)]))

(define (pass2 node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
    [(s:primapp primitive args loc)
     (match primitive
       [(struct sines-early-primitive [_ transformer _ _ _])
        (apply transformer loc args)]
       [_ (s:primapp primitive (map pass2 args) loc)])]
    [(s:app op args loc)                  (s:app (pass2 op) (map pass2 args) loc)]
    [(s:begin body loc)                   (s:begin (map pass2 body) loc)]
    [(s:lambda ids rest-id body loc)      (s:lambda ids rest-id (pass2 body) loc)]
    [(s:dispatch-lambda procs loc)        (s:dispatch-lambda (map pass2 procs) loc)]
    [(s:if test then else loc)            (s:if (pass2 test) (pass2 then) (pass2 else) loc)]
    [(s:let-values idss vals body loc)    (s:let-values idss (map pass2 vals) (pass2 body) loc)]
    [(s:letrec-values idss vals body loc) (s:letrec-values idss (map pass2 vals) (pass2 body) loc)]
    [(s:define-values ids value loc)      (s:define-values ids (pass2 value) loc)]
    [(s:set! var value loc)               (s:set! (pass2 var) (pass2 value) loc)]
    [(s:wcm key value expr loc)           (s:wcm (pass2 key) (pass2 value) (pass2 expr) loc)]
    [(s:program body loc)                 (s:program (map pass2 body) loc)]))

(provide/contract
 [transform (stx? . -> . stx?)])
