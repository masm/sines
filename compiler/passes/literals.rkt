#lang scheme

(require
 "../state-monad.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.rkt")

(define-struct state (env vals))

(define (transform node)
  (inject (literals node) (make-state (make-immutable-hash '()) '())))

(define (maybe-extend-env loc value)
  (lambda (state)
     (let ([env (state-env state)]
           [vals (state-vals state)])
       (let ([id (hash-ref env value (lambda () #f))])
         (if id
             (cons (s:global-ref id)
                   (make-state env vals))
             (let ([id (new-global-id "literal")])
               (cons (s:global-ref id loc)
                     (make-state (hash-set env value id)
                                 (cons value vals)))))))))

(define (maybe-extend-env/global literal id)
  (lambda (state)
     (let ([env (state-env state)]
           [vals (state-vals state)]
           [value (literal-stx-value literal)])
       (cons literal
             (if (hash-ref env value (lambda () #f))
                 state
                 (make-state (hash-set env value id)
                             vals))))))

(define (char/pair/vector? value)
  (or (char? value) (pair? value) (vector? value)))

(define (literals node)
  (match node
    [(s:literal (? char/pair/vector? value) loc)
     (maybe-extend-env loc value)]
    [(or (s:literal) (s:lexical-ref) (s:global-ref))
     (return node)]
    [(struct primapp-stx [loc transformer args])
     (monad-seq literals args (lambda (nodes) (return (make-primapp-stx loc transformer nodes))))]
    [(struct app-stx [loc op args])
     (let-m ([op (literals op)])
       (monad-seq literals args (lambda (nodes) (return (make-app-stx loc op nodes)))))]
    [(struct begin-stx [loc body])
     (monad-seq literals body (lambda (nodes) (return (make-begin-stx loc nodes))))]
    [(struct begin/var-stx [loc ids body])
     (monad-seq literals body (lambda (nodes) (return (make-begin/var-stx loc ids nodes))))]
    [(struct define-values-stx [loc ids value])
     (let-m ([value (cond [(literal-stx? value)
                           (unless (and (not (null? ids)) (null? (cdr ids)))
                             (error 'literals "multiple-values definition for literal, in ~A" node))
                           (maybe-extend-env/global value (car ids))]
                          [else (literals value)])])
       (return (make-define-values-stx loc ids value)))]
    [(struct if-stx [loc test then else])
     (let*-m ([test (literals test)]
              [then (literals then)]
              [else (literals else)])
       (return (make-if-stx loc test then else)))]
    [(struct lambda-stx [loc ids rest-id body])
     (let-m ([body (literals body)])
       (return (make-lambda-stx loc ids rest-id body)))]
    [(struct dispatch-lambda-stx [loc procs])
     (monad-seq literals procs (lambda (nodes) (return (make-dispatch-lambda-stx loc nodes))))]
    [(struct lexical-init-stx [loc id val])
     (let-m ([val (literals val)])
       (return (make-lexical-init-stx loc id val)))]
    [(s:loop ids values body loc)
     (monad-seq literals values
                (lambda (values)
                  (let-m ([body (literals body)])
                    (return (s:loop ids values body loc)))))]
    [(s:iterate args loc)
     (monad-seq literals args
                (lambda (args)
                  (return (s:iterate args loc))))]
    [(struct program/var-stx [loc global-ids lexical-ids body])
     (>>=/sample/reset
      (literals (car body))
      (let loop ([body (cdr body)]
                 [nodes '()])
        (lambda (node state)
          (let ([env (state-env state)]
                [vals (state-vals state)])
            (values
             (if (null? body)
                 (return (s:program/var (append global-ids (hash-map env (lambda (_ id) id)))
                                        lexical-ids
                                        (reverse
                                         (cons node
                                               (append
                                                (map (lambda (val)
                                                       (s:global-init (hash-ref env val)
                                                                      (s:literal val)))
                                                     vals)
                                                nodes)))
                                        loc))
                 (>>=/sample/reset
                  (literals (car body))
                  (loop (cdr body)
                        (cons node
                              (append (map (lambda (val)
                                             (s:global-init (hash-ref env val)
                                                            (s:literal val)))
                                           vals)
                                      nodes)))))
             (make-state env '()))))))]
    [_ (error "literals: not matched" node)]))

(provide/contract
 [transform (stx? . -> . stx?)])

