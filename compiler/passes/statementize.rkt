#lang scheme

(require
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt"))

(define (transform node)
  (statement node))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (statement node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) (s:non-tail node)]
    [(s:primapp primitive args loc)  (s:non-tail (s:primapp primitive (map expression args) loc))]
    [(s:app op args loc)             (s:non-tail (s:app (expression op) (map expression args) loc))]
    [(s:tail app loc)                (s:tail (expression app) loc)]
    [(s:begin body loc)              (s:begin (map statement body) loc)]
    [(s:if-true test then loc)       (s:if-true (expression test) (statement then) loc)]
    [(s:if test then else loc)       (s:if (expression test) (statement then) (statement else) loc)]
    [(s:lambda ids rest-id body loc) (s:non-tail (s:lambda ids rest-id (statement body) loc))]
    [(s:dispatch-lambda procs loc)   (s:non-tail (s:dispatch-lambda (map expression procs) loc))]
    [(s:lexical-init id value loc)   (s:non-tail (s:lexical-init id (expression value) loc))]
    [(s:global-init id value loc)    (s:non-tail (s:global-init id (expression value) loc))]
    [(s:loop ids values body loc)    (s:non-tail (s:loop ids (map expression values) (statement body) loc))]
    [(s:iterate args loc)            (s:non-tail (s:iterate (map expression args) loc))]
    [(s:blocks ids blocks loc)       (s:blocks ids (map statement blocks) loc)]
    [(s:block frames body loc)       (s:block frames (map statement body) loc)]
    [(s:begin/const/var const-ids const-values ids body loc) (s:begin/const/var const-ids (map expression const-values) ids (map statement body) loc)]
    [(s:program/const/var const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body loc)
     (s:program/const/var const-global-ids (map expression const-global-values) const-lexical-ids (map expression const-lexical-values) global-ids lexical-ids (map statement body) loc)]
    [_ (error 'statementize "statement: not matched ~A" node)]))

(define (expression node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
    [(s:primapp primitive args loc)  (s:primapp primitive (map expression args) loc)]
    [(s:app op args loc)             (s:app (expression op) (map expression args) loc)]
    [(s:begin body loc)              (s:begin (map expression body) loc)]
    [(s:if-true test then loc)       (s:if-true (expression test) (expression then) loc)]
    [(s:if test then else loc)       (s:if (expression test) (expression then) (expression else) loc)]
    [(s:lambda ids rest-id body loc) (s:lambda ids rest-id (statement body) loc)]
    [(s:dispatch-lambda procs loc)   (s:dispatch-lambda (map expression procs) loc)]
    [(s:lexical-init id value loc)   (s:lexical-init id (expression value) loc)]
    [(s:loop ids values body loc)    (s:loop ids (map expression values) (statement body) loc)]
    [(s:iterate args loc)            (s:iterate (map expression args) loc)]
    [_ (error 'statementize "expression: not matched ~A" node)]))
