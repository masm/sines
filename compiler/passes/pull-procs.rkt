#lang scheme

(require
 "../primitives.rkt"
 "../state-monad.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt"))

;; TODO: finish

(define-struct state ())

(define (transform node)
  (inject (pull-procs node) (make-state)))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (pull-procs node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref))
     (return node)]
    [(s:primapp primitive args loc)
     (monad-seq pull-procs args (lambda (nodes) (return (s:primapp primitive nodes loc))))]
    [(s:app op args loc)
     (let-m ([op (pull-procs op)])
       (monad-seq pull-procs args (lambda (nodes) (return (s:app op nodes loc)))))]
    [(s:tail app loc)
     (let-m ([app (pull-procs app)])
       (return (s:tail app loc)))]
    [(s:begin body loc) ;; (list init ... last)
     (monad-seq pull-procs body
                (lambda (nodes)
                  (return (s:begin nodes loc))))]
    [(s:begin/var ids body loc) ;; (list init ... last)
     (monad-seq pull-procs body
                (lambda (nodes)
                  (let-values ([(const-lexical-inits nodes)
                                (partition const-lexical-init? nodes)])
                    (let ([const-ids (map lexical-init-stx-id const-lexical-inits)]
                          [const-values (map lexical-init-stx-value const-lexical-inits)])
                      (let ([ids (remove* const-ids ids)])
                        (return (s:begin/const/var const-ids const-values ids nodes loc)))))))]
    [(s:if test then else loc)
     (let*-m ([test (pull-procs test)]
              [then (pull-procs then)]
              [else (pull-procs else)])
       (return (s:if test then else loc)))]
    [(s:lambda ids rest-id body loc)
     (let-m ([body (pull-procs body)])
       (return (s:lambda ids rest-id body loc)))]
    [(s:dispatch-lambda procs loc)
     (monad-seq pull-procs procs (lambda (nodes) (return (s:dispatch-lambda nodes loc))))]
    [(s:lexical-init id value loc)
     (let-m ([value (pull-procs value)])
       (return (s:lexical-init id value loc)))]
    [(s:global-init id value loc)
     (let-m ([value (pull-procs value)])
       (return (s:global-init id value loc)))]
    [(s:loop ids values body loc)
     (monad-seq pull-procs values
                (lambda (values)
                  (let-m ([body (pull-procs body)])
                    (return (s:loop ids values body loc)))))]
    [(s:iterate args loc)
     (monad-seq pull-procs args
                (lambda (args)
                  (return (s:iterate args loc))))]
    [(s:program/var global-ids lexical-ids body loc)
     (monad-seq pull-procs body
                (lambda (nodes)
                  (let-values ([(const-global-inits nodes) (partition const-global-init? nodes)])
                    (let-values ([(const-lexical-inits nodes) (partition const-lexical-init? nodes)])
                      (let ([const-global-ids (map global-init-stx-id const-global-inits)]
                            [const-global-values (map global-init-stx-value const-global-inits)]
                            [const-lexical-ids (map lexical-init-stx-id const-lexical-inits)]
                            [const-lexical-values (map lexical-init-stx-value const-lexical-inits)])
                        (let ([global-ids (remove* const-global-ids global-ids)]
                              [lexical-ids (remove* const-lexical-ids lexical-ids)])
                          (return (s:program/const/var const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids nodes loc))))))))]
    [_ (error 'pull-procs "not matched ~A" node)]))

(define (const-global-init? node)
  (match node
    [(s:global-init _ value) (const-value-stx? value)]
    [_ #f]))

(define (const-lexical-init? node)
  (match node
    [(s:lexical-init _ value) (const-value-stx? value)]
    [_ #f]))

(define (const-value-stx? value)
  (match value
    [(or (s:lambda) (s:dispatch-lambda) (s:literal)) #t]
    [(s:primapp primitive _) (sines-primitive-denotation primitive)]
    [_ #f]))
