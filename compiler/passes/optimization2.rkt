#lang scheme

(require "../syntax.rkt"
         (prefix-in s: "../syntax2.rkt")
         (prefix-in l: "../primitives.rkt")
         (prefix-in l: "../primitives2.rkt"))

(define (transform node)
  (contraction node))

(define (contraction node)
  (match node
    [(or (struct literal-stx _)
         (struct lexical-ref-stx _)
         (struct global-ref-stx _))
     node]
    [(struct primapp-stx [loc transformer args])
     (make-primapp-stx loc transformer (map contraction args))]
    [(struct app-stx [loc op args])
     (make-app-stx loc (contraction op) (map contraction args))]
    [(struct begin-stx [loc (list init ... last)])
     (make-begin-stx loc (append (filter may-have-side-effects? (map contraction-init init))
                                     (list (contraction last))))]
    [(struct begin/var-stx [loc ids (list init ... last)])
     (make-begin/var-stx loc ids
                         (append (filter may-have-side-effects? (map contraction-init init))
                                 (list (contraction last))))]
    [(struct if-stx [loc test then else])
     (make-if-stx loc (contraction test) (contraction then) (contraction else))]
    [(struct lambda-stx [loc ids rest-id body])
     (make-lambda-stx loc ids rest-id (contraction body))]
    [(struct dispatch-lambda-stx [loc procs])
     (make-dispatch-lambda-stx loc (map contraction procs))]
    [(struct lexical-init-stx [loc id value])
     (make-lexical-init-stx loc id (contraction value))]
    [(struct global-init-stx [loc id value])
     (make-global-init-stx loc id (contraction value))]
    [(s:loop ids values body loc)
     (s:loop ids (map contraction values) (contraction body) loc)]
    [(s:iterate args loc)
     (s:iterate (map contraction args) loc)]
    [(struct program/var-stx [loc global-ids lexical-ids body])
     (make-program/var-stx loc global-ids lexical-ids
                           (filter may-have-side-effects? (map contraction-init body)))]
    [_ (error "optimization2 contraction: not matched" node)]))

(define (contraction-init node)
  (match node
    [(s:begin body)
     (let ([body (filter  may-have-side-effects? (map contraction-init body))])
       (cond [(null? body)
              (s:primapp l:%%void '())]
             [(null? (cdr body))
              (car body)]
             [else (s:begin body)]))]
    [_ (contraction node)]))

(define (may-have-side-effects? node)
  (match node
    [(or (struct literal-stx _)
         (struct lexical-ref-stx _)
         (struct global-ref-stx _)
         (struct lambda-stx _)
         (struct dispatch-lambda-stx _))
     #f]
    [(or (struct app-stx _)
         (struct lexical-init-stx _)
         (struct global-init-stx _))
     #t]
    [(struct primapp-stx [_ op args])
     (or (not (l:sines-primitive-side-effects-free? op))
         (ormap may-have-side-effects? args))]
    [(struct begin-stx [_ body])
     (ormap may-have-side-effects? body)]
    [(struct begin/var-stx [_ _ body])
     (ormap may-have-side-effects? body)]
    [(struct if-stx [_ test then else])
     (or (may-have-side-effects? test)
         (may-have-side-effects? then)
         (may-have-side-effects? else))]
    [(s:loop _ values body)
     (or (ormap may-have-side-effects? values)
         (may-have-side-effects? body))]
    [(s:iterate args)
     (ormap may-have-side-effects? args)]
    [_ (error "optimization2 may-have-side-effects?: not matched" node)]))

(provide/contract
 [transform (stx? . -> . stx?)])
