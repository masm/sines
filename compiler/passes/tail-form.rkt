#lang scheme

(require
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt"))

(define (transform node)
  (non-tail node))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (non-tail node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
    [(s:primapp transformer args loc) (s:primapp transformer (map non-tail args) loc)]
    [(s:app op args loc)              (s:app (non-tail op) (map non-tail args) loc)]
    [(s:begin body loc)               (s:begin (map non-tail body) loc)]
    [(s:begin/var ids body loc)       (s:begin/var ids (map non-tail body) loc)]
    [(s:if test then else loc)        (s:if (non-tail test) (non-tail then) (non-tail else) loc)]
    [(s:lambda ids rest-id body loc)  (s:lambda ids rest-id (tail body) loc)]
    [(s:dispatch-lambda procs loc)    (s:dispatch-lambda (map non-tail procs) loc)]
    [(s:lexical-init id value loc)    (s:lexical-init id (non-tail value) loc)]
    [(s:global-init id value loc)     (s:global-init id (non-tail value) loc)]
    [(s:loop ids values body loc)     (s:loop ids (map non-tail values) (tail body) loc)]
    [(s:iterate args loc)             (s:iterate (map non-tail args) loc)]
    [(s:program/var global-ids lexical-ids body loc) (s:program/var global-ids lexical-ids (map non-tail body) loc)]
    [_ (error 'tail-form "non-tail: not matched ~A" node)]))

(define (tail node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) (s:tail node)]
    [(s:primapp transformer args loc)           (s:tail (s:primapp transformer (map non-tail args) loc))]
    [(s:app op args loc)                        (s:tail (s:app (non-tail op) (map non-tail args) loc))]
    [(s:begin (list init ... last) loc)         (s:begin (append (map non-tail init) (list (tail last))) loc)]
    [(s:begin/var ids (list init ... last) loc) (s:begin/var ids (append (map non-tail init) (list (tail last))) loc)]
    [(s:if test then else loc)                  (s:if (non-tail test) (tail then) (tail else) loc)]
    [(s:lambda ids rest-id body loc)            (s:tail (s:lambda ids rest-id (tail body) loc))]
    [(s:dispatch-lambda procs loc)              (s:tail (s:dispatch-lambda (map non-tail procs) loc))]
    [(s:loop ids values body loc)               (s:tail (s:loop ids (map non-tail values) (tail body) loc))]
    [(s:iterate args loc)                       (s:tail (s:iterate (map non-tail args) loc))]
    [_ (error 'tail-form "not matched ~A" node)]))
