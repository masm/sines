#lang scheme

(require "../syntax.rkt"
         (prefix-in s: "../syntax2.rkt"))

(define (transform node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref))
     node]
    [(s:primapp primitive args loc)
     (s:primapp primitive (map transform args) loc)]
    [(s:app op args loc)
     (s:app (transform op) (map transform args) loc)]
    [(s:tail expr loc)
     (s:tail (transform expr) loc)]
    [(s:begin body loc)
     (s:begin (map transform body) loc)]
    [(s:begin/const/var const-ids const-values ids body loc)
     (s:begin/const/var const-ids (map transform const-values) ids
                        (append-map flatten-begin (map transform body))
                        loc)]
    [(s:if test then else loc)
     (s:if (transform test) (transform then) (transform else) loc)]
    [(s:lambda ids rest-id body loc)
     (s:lambda ids rest-id (transform body) loc)]
    [(s:dispatch-lambda procs loc)
     (s:dispatch-lambda (map transform procs) loc)]
    [(s:lexical-init id value loc)
     (s:lexical-init id (transform value) loc)]
    [(s:global-init id value loc)
     (s:global-init id (transform value) loc)]
    [(s:loop ids values body loc)
     (s:loop ids (map transform values) (transform body) loc)]
    [(s:iterate args loc)
     (s:iterate (map transform args) loc)]
    [(s:program/const/var const-global-ids const-global-values
                          const-lexical-ids const-lexical-values
                          global-ids lexical-ids body loc)
     (s:program/const/var const-global-ids (map transform const-global-values)
                          const-lexical-ids (map transform const-lexical-values)
                          global-ids lexical-ids
                          (append-map flatten-begin (map transform body))
                          loc)]
    [_ (error "label-bodies: not matched" node)]))

(define (flatten-begin node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:primapp) (s:app) (s:tail) (s:lambda) (s:dispatch-lambda) (s:iterate))
     (list node)]
    [(s:loop) ;; This may have begin forms inside, but everything is a primitive, so just ignore it
     (list node)]
    [(s:begin body)
     (append-map flatten-begin body)]
    [(s:if test then else loc)
     (interleave (map (lambda (x) (s:if-true test x loc))
                      (flatten-begin then))
                 (map (lambda (x) (s:if-false test x loc))
                      (flatten-begin else)))]
    [(s:lexical-init id value loc)
     (match value
       [(s:begin (list init ... last) loc2)
        (flatten-begin (s:begin (append init (list (s:lexical-init id last loc))) loc2))]
       [(s:if test then else loc2)
        (flatten-begin (s:if test
                             (s:lexical-init id then loc)
                             (s:lexical-init id else loc)
                             loc2))]
       [_ (list node)])]
    [(s:global-init id value loc)
     (match value
       [(s:begin (list init ... last) loc2)
        (flatten-begin (s:begin (append init (list (s:global-init id last loc))) loc2))]
       [(s:if test then else loc2)
        (flatten-begin (s:if test
                             (s:global-init id then loc)
                             (s:global-init id else loc)
                             loc2))]
       [_ (list node)])]
    [_ (error "flatten-begin: not matched" node)]))

(define (interleave x y)
  (if (null? x)
      y
      (cons (car x) (interleave y (cdr x)))))

(provide/contract
 [transform (stx? . -> . stx?)])
