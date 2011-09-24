#lang scheme/base

(require
 scheme/base
 scheme/contract
 scheme/list
 scheme/match
 "../primitives.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../store.rkt"
 "../utils.rkt"
 "../backend.rkt")

(define (transform node)
  (inline node .001 '() (definitions-hash node)))

(define (decremented k)
  (/ k 2))

(provide/contract
 [transform (stx? . -> . stx?)])

(define (definitions-hash node)
  (match node
    [(s:program body)
     (let ([hash (make-hash)])
       (for-each (lambda (node)
                   (match node
                     [(s:define-values (list id) value)
                      (hash-set! hash id value)]
                     [_ (void)]))
                 body)
       hash)]
    [_ (error "inline hash-of-definitions: not matched" node)]))

(define (code-size node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) 0]
    [(s:primapp op args)            (list-code-size args)]
    [(s:app op args)                (+ 100 (code-size op) (list-code-size args))]
    [(s:begin body)                 (list-code-size body)]
    [(s:if test then else)          (+ (code-size test) (code-size then) (code-size else))]
    [(s:lambda ids rest-id body)    (+ 100 (code-size body))]
    [(s:dispatch-lambda procs)      (+ 100 (list-code-size procs))]
    [(s:let-values ids values body) (+ (list-code-size values) (code-size body))]
    [(s:fix ids procs body)         (+ (list-code-size procs)  (code-size body))]
    [(s:wcm key value expr)         (+ 100 (code-size key) (code-size value) (code-size expr))]
    [_ (error "inline: not matched" node)]))

(define (call-code-size args)
  (+ 100 (list-code-size args)))

(define (list-code-size nodes)
  (foldl + 0 (map code-size nodes)))

(define (inline node k S defs)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
    [(s:primapp op args loc)            (s:primapp op (list-inline args k S defs) loc)]
    [(and (s:app op args loc) call)
     (let ([op (inline op k S defs)]
           [args (list-inline args k S defs)])
       (match op
         [(s:global-ref id) (=> fail)
          (or (maybe-inlined-call id args loc k S defs)
              (fail))]
         [(s:lexical-ref id) (=> fail)
          (or (maybe-inlined-call id args loc k S defs)
              (fail))]
         [_ (s:app op args loc)]))]
    [(s:begin body loc)                 (s:begin (list-inline body k S defs) loc)]
    [(s:define-values ids value loc)    (s:define-values ids (inline value k S defs) loc )]
    [(s:if test then else loc)          (s:if (inline test k S defs) (inline then k S defs) (inline else k S defs) loc)]
    [(s:lambda ids rest-id body loc)    (s:lambda ids rest-id (inline body k S defs) loc)]
    [(s:dispatch-lambda procs loc)      (s:dispatch-lambda (list-inline procs k S defs) loc)]
    [(s:let-values ids values body loc)
     (let ([new-defs (extended-defs defs ids values)])
       (s:let-values ids (list-inline values k S defs) (inline body k S new-defs) loc))]
    [(s:fix ids procs body loc)
     (let ([new-defs (extended-defs defs ids procs) ])
       (s:fix ids (list-inline procs k S new-defs) (inline body k S new-defs) loc))]
    [(s:wcm key value expr loc)         (s:wcm (inline key k S defs) (inline value k S defs) (inline expr k S defs) loc)]
    [(s:program body loc)               (s:program (list-inline body k S defs)  loc)]
    [_ (error "inline: not matched" node)]))

(define (extended-defs defs ids values)
  (for-each (lambda (ids value)
              (match ids
                [(list id) (hash-set! defs id value)]
                [_ (void)]))
            ids values)
  defs)

(define (list-inline nodes k S defs)
  (map (lambda (node) (inline node k S defs)) nodes))

(define (maybe-inlined-call id args loc k S defs)
  (cond [(hash-ref defs id (lambda () #f))
         => (lambda (value)
              (let-values ([(value size) (cond [(pair? value) (values (car value) (cdr value))]
                                               [else
                                                (match value
                                                  [(s:lambda _ _ body)
                                                   (let ([size (code-size body)])
                                                     (hash-set! defs id (cons value size))
                                                     (values value size))]
                                                  [_ (values value #f)])])])
                (match value
                  [(s:lambda _ _ body)
                   (and (< (or size (code-size body)) (* k (call-code-size args)))
                        (inlined-call value args loc k S defs))]
                  [(s:dispatch-lambda)
                   (let ([proc (dispatch-lambda/arity->lambda value (length args))])
                     (and (< (code-size (lambda-stx-body proc)) (* k (call-code-size args)))
                          (inlined-call proc args loc k S defs)))]
                  [_ #f])))]
        [else #f]))

(define (inlined-call proc args loc k S defs)
  (inline (app->let-node/rename loc proc args) (decremented k) S defs))
