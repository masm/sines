#lang scheme

(require "../syntax.rkt"
         (prefix-in s: "../syntax2.rkt")
         (prefix-in l: "../primitives.rkt")
         (prefix-in l: "../primitives2.rkt"))

(define (transform node)
  (contraction node))

(define (contraction node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
    [(s:primapp primitive args loc) (s:primapp primitive (map contraction args) loc)]
    [(s:app op args loc)            (s:app (contraction op) (map contraction args) loc)]
    [(s:tail app loc)               (s:tail (contraction app) loc)]
    [(s:begin body loc)
     (let ([nodes (map contraction (doit body))])
       (if (null? (cdr nodes))
           (car nodes)
           (s:begin nodes loc)))]
    [(s:if-true test then loc)      (s:if-true (contraction test) (contraction then) loc)]
    [(s:if-false test then loc)     (s:if-true (s:primapp l:%%not (list (contraction test))) (contraction then) loc)]
    [(s:if test then else loc)      (s:if (contraction test) (contraction then) (contraction else) loc)]
    [(s:lambda ids rest-id
               (s:begin/const/var const-ids const-values ids2 body loc2)
               loc)
     (s:lambda ids rest-id
               (s:begin/const/var const-ids (map contraction const-values)
                                  ids2 (map contraction body) loc2)
               loc)]
    [(s:dispatch-lambda procs loc)  (s:dispatch-lambda (map contraction procs) loc)]
    [(s:lexical-init id value loc)  (s:lexical-init id (contraction value) loc)]
    [(s:global-init id value loc)   (s:global-init id (contraction value) loc)]
    [(s:blocks ids blocks loc)      (s:blocks ids (map contraction blocks) loc)]
    [(s:block frames body loc)      (s:block frames (map contraction (doit body)) loc)]
    [(s:loop ids values body loc)   (s:loop ids (map contraction values) (contraction body) loc)]
    [(s:iterate args loc)           (s:iterate (map contraction args) loc)]
    [(s:program/const/var const-global-ids const-global-values
                          const-lexical-ids const-lexical-values
                          global-ids lexical-ids body loc)
     (s:program/const/var const-global-ids (map contraction const-global-values)
                          const-lexical-ids (map contraction const-lexical-values)
                          global-ids lexical-ids (map contraction body) loc)]
    [_ (error "optimization3: not matched" node)]))

(define (doit nodes)
  (let loop ([rest (cdr nodes)]
             [acc (list (car nodes))])
    (if (null? rest)
        (reverse acc)
        (loop (cdr rest)
              (match (cons (car acc) (car rest))
                [(and (cons (s:if-true test then loc) (s:if-true test2 then2))) (=> retry)
                 (unless (eq? test test2) (retry))
                 (cons (s:if-true test (join then then2) loc)
                       (cdr acc))]
                [(and (cons (s:if-true test then loc) (s:if-false test2 else))) (=> retry)
                 (unless (eq? test test2) (retry))
                 (cons (s:if test then else loc)
                       (cdr acc))]
                [(and (cons (s:if-false test else loc) (s:if-true test2 then))) (=> retry)
                 (unless (eq? test test2) (retry))
                 (cons (s:if test then else loc)
                       (cdr acc))]
                [(and (cons (s:if-false test else loc) (s:if-false test2 else2))) (=> retry)
                 (unless (eq? test test2) (retry))
                 (cons (s:if-false test (join else else2) loc)
                       (cdr acc))]
                [(and (cons (s:if test then else loc) (s:if-true test2 then2))) (=> retry)
                 (unless (eq? test test2) (retry))
                 (cons (s:if test (join then then2) else loc)
                       (cdr acc))]
                [(and (cons (s:if test then else loc) (s:if-false test2 else2))) (=> retry)
                 (unless (eq? test test2) (retry))
                 (cons (s:if test then (join else else2) loc)
                       (cdr acc))]
                [_ (cons (car rest) acc)])))))

(define (join a b)
  (match (cons a b)
    [(cons (s:begin a) (s:begin b))
     (s:begin (append a b))]
    [(cons (s:begin a) b)
     (s:begin (append a (list b)))]
    [(cons a (s:begin b))
     (s:begin (cons a b))]
    [_ (s:begin (list a b))]))

(provide/contract
 [transform (stx? . -> . stx?)])
