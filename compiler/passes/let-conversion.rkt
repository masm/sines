#lang scheme

(require
 (prefix-in p: "../primitives2.rkt")
 "../state-monad.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.rkt")

(define (transform node)
  (inject (let-conversion (rename node (make-immutable-hash '())))
          initial-state))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define initial-state '())

(define (state-ids x) x)

(define (clone-state/add-ids old-state new-ids)
  (append new-ids old-state))

(define (extend-env env ids)
  (foldr (lambda (id env)
           (hash-set env id (lexical-id-clone id)))
         env
         ids))

(define (rename node env)
  (match node
    [(or (s:literal) (s:global-ref)) node]
    [(s:lexical-ref id loc)           (s:lexical-ref (hash-ref env id) loc)]
    [(s:primapp transformer args loc) (s:primapp transformer (list-rename args env) loc)]
    [(s:app op args loc)              (s:app (rename op env) (list-rename args env) loc)]
    [(s:begin body loc)               (s:begin (list-rename body env) loc)]
    [(s:define-values ids value loc)  (s:define-values ids (rename value env) loc)]
    [(s:if test then else loc)        (s:if (rename test env) (rename then env) (rename else env) loc)]
    [(s:dispatch-lambda procs loc)    (s:dispatch-lambda (list-rename procs env) loc)]
    [(s:lambda ids rest-id body loc)
     (let ([new-env (extend-env env (if rest-id (cons rest-id ids) ids))])
       (s:lambda (map (lambda (id) (hash-ref new-env id)) ids)
                 (and rest-id (hash-ref new-env rest-id))
                 (rename body new-env)
                 loc))]
    [(s:let-values idss values body loc)
     (let ([new-env (extend-env env (apply append idss))])
       (s:let-values (map (lambda (ids) (map (lambda (id) (hash-ref new-env id)) ids)) idss)
                     (list-rename values env)
                     (rename body new-env)
                     loc))]
    [(s:fix ids procs body loc)
     (let ([new-env (extend-env env ids)])
       (s:fix (map (lambda (id) (hash-ref new-env id)) ids)
              (list-rename procs new-env)
              (rename body new-env)
              loc))]
    [(s:loop ids values body loc)
     (let ([new-env (extend-env env ids)])
       (s:loop (map (lambda (id) (hash-ref new-env id)) ids) (list-rename values env) (rename body new-env) loc))]
    [(s:iterate args loc)         (s:iterate (list-rename args env) loc)]
    [(s:program body loc) (s:program (list-rename body env) loc)]
    [_ (error 'let-conversion "rename: not matched" node)]))

(define (list-rename l env)
  (map (lambda (node) (rename node env)) l))

(define (let-conversion node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) (return node)]
    [(s:primapp transformer args loc)
     (monad-seq let-conversion args
                (lambda (nodes) (return (s:primapp transformer nodes loc))))]
    [(s:app op args loc)
     (let-m ([op (let-conversion op)])
       (monad-seq let-conversion args
                  (lambda (nodes) (return (s:app op nodes loc)))))]
    [(s:begin body loc)
     (monad-seq let-conversion body
                (lambda (nodes) (return (s:begin nodes loc))))]
    [(s:define-values ids value loc)
     (let-m ([value (let-conversion value)])
       (return (s:define-values ids value loc)))]
    [(s:if test then else loc)
     (let*-m ([test (let-conversion test)]
              [then (let-conversion then)]
              [else (let-conversion else)])
       (return (s:if test then else loc)))]
    [(s:lambda ids rest-id body loc)
     (lambda (old-state)
       (let ([m (let-conversion body)])
         (match (m initial-state)
           [(cons new-body new-state)
            (cons (s:lambda ids rest-id
                                   (s:begin/var (state-ids new-state) (list new-body)) loc)
                  old-state)])))]
    [(s:dispatch-lambda procs loc)
     (monad-seq let-conversion procs
                (lambda (nodes) (return (s:dispatch-lambda nodes loc))))]
    [(s:let-values idss vals body loc)
     (monad-seq let-conversion vals
                (lambda (vals)
                  (lambda (old-state)
                    (let ([m (let-conversion body)])
                      (match (m old-state)
                        [(cons new-body new-state)
                         (let ([init-ids+nodes (map (lambda (ids val)
                                                      (cond [(null? ids)       (cons #f (list val))]
                                                            [(null? (cdr ids)) (cons #f (list (s:lexical-init (car ids) val)))]
                                                            [else
                                                             (let ([new-id (new-lexical-id)])
                                                               (cons new-id
                                                                     (cons (s:lexical-init new-id val)
                                                                           (for/list ([id (in-list ids)]
                                                                                      [i (in-range (length ids))])
                                                                             (s:lexical-init id (s:primapp p:%%from-values (list (s:lexical-ref new-id) (s:literal i))))))))]))
                                                    idss vals)])
                           (let ([new-ids (filter-map car init-ids+nodes)]
                                 [init-nodes (append-map cdr init-ids+nodes)])
                             (cons (s:begin (append init-nodes (list new-body)) loc)
                                   (clone-state/add-ids new-state (apply append new-ids idss)))))])))))]
    [(s:fix ids procs body loc)
     (monad-seq let-conversion procs
                (lambda (nodes)
                  (lambda (old-state)
                    (let ([m (let-conversion body)])
                      (match (m old-state)
                        [(cons new-body new-state)
                         (let ([init-nodes (map (lambda (id val) (s:lexical-init id val))
                                                ids nodes)])
                           (cons (s:begin (append init-nodes (list new-body)) loc)
                                 (clone-state/add-ids new-state ids)))])))))]
    [(s:loop ids vals body loc)
     (monad-seq let-conversion vals
                (lambda (vals)
                  (lambda (old-state)
                    (let ([m (let-conversion body)])
                      (match (m old-state)
                        [(cons body new-state)
                         (cons (s:loop ids vals body loc)
                               (clone-state/add-ids new-state ids))])))))]
    [(s:iterate args loc)
     (monad-seq let-conversion args
                (lambda (args) (return (s:iterate args loc))))]
    [(s:program body loc)
     (monad-seq let-conversion body
                (lambda (nodes)
                  (lambda (state)
                    (cons (s:program/var '() (state-ids state) nodes loc)
                          state))))]
    [_ (error "let-conversion: not matched" node)]))
