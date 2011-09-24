#lang scheme/base

(require
 scheme/base
 scheme/contract
 scheme/list
 scheme/match
 "../primitives.rkt"
 "../syntax.rkt"
 "../store.rkt"
 "../constant-folding-defs.rkt"
 "../utils.rkt"
 (prefix-in l: "../../private/library.ss")
 (prefix-in s: "../syntax2.rkt")
 "../backend.rkt")

(define (transform node)
  (let-values ([(lexical-use-hash global-use-hash) (use-count-hashes node)])
    (contraction node lexical-use-hash global-use-hash
                 (make-immutable-hasheq '())
                 (global-replacement node global-use-hash))))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (use-count-hashes node)
  (let ([lexical-use-hash (make-hasheq)]
        [global-use-hash (make-hash)])
    (define (reset-lexical! id)
      (cond [(hash-ref lexical-use-hash id (lambda () #f)) => (lambda (v) (error 'contraction "use-count-hashes: lexical already in hash ~A" id))])
      (hash-set! lexical-use-hash id 0))
    (define (inc-lexical-use! id)
      (hash-set! lexical-use-hash id (add1 (hash-ref lexical-use-hash id))))
    (define (reset-global! id)
      (cond [(hash-ref global-use-hash id (lambda () #f)) => (lambda (v) (error 'contraction "use-count-hashes: global already in hash ~A" id))])
      (hash-set! global-use-hash id 0))
    (define (inc-global-use! id)
      (hash-set! global-use-hash id (add1 (hash-ref global-use-hash id))))
    (let loop ([node node])
      (match node
        [(s:literal) (void)]
        [(s:global-ref id)             (inc-global-use! id)]
        [(s:lexical-ref  id)           (inc-lexical-use! id)]
        [(s:primapp  _ args)           (for-each loop args)]
        [(s:app  op args)              (loop op) (for-each loop args)]
        [(s:begin  body)               (for-each loop body)]
        [(s:dispatch-lambda  procs)    (for-each loop procs)]
        [(s:define-values  _ value)    (loop value)]  ; reset-global! was called in the program node
        [(s:if test then else)         (loop test) (loop then) (loop else)]
        [(s:lambda ids rest-id body)
         (for-each reset-lexical! ids)
         (when rest-id (reset-lexical! rest-id))
         (loop body)]
        [(s:let-values ids vals body)
         (for-each (lambda (ids) (for-each reset-lexical! ids)) ids)
         (for-each loop vals)
         (loop body)]
        [(s:fix ids procs body)
         (for-each reset-lexical! ids)
         (for-each loop procs)
         (loop body)]
        [(s:wcm key value expr)        (loop key) (loop value) (loop expr)]
        [(s:loop ids vals body)
         (for-each loop vals)
         (for-each reset-lexical! ids)
         (loop body)]
        [(s:iterate args)
         (for-each  loop args)]
        [(s:program body)
         (for-each (lambda (n)
                     (match n
                       [(s:define-values ids _) (for-each reset-global! ids)]
                       [_ (void)]))
                   body)
         (map loop body)]
        [_ (error 'contraction "use-count-hashes: not matched ~A" node)]))
    (values lexical-use-hash global-use-hash)))

(define (update-lexical-use-count-hash! lexical-use-hash node)
  (define (reset-lexical! id)
    (cond [(hash-ref lexical-use-hash id (lambda () #f)) => (lambda (v) (error 'contraction "update-lexical-use-count-hash!: çexical already in hash ~A" id))])
    (hash-set! lexical-use-hash id 0))
  (define (inc-lexical-use! id)
    (hash-set! lexical-use-hash id (add1 (hash-ref lexical-use-hash id))))
  (let loop ([node node])
    (match node
      [(s:literal) (void)]
      [(s:global-ref id)                                (void)]
      [(s:lexical-ref  id)                (inc-lexical-use! id)]
      [(s:primapp  _ args)                (for-each loop args)]
      [(s:app op args)                   (loop op) (for-each loop args)]
      [(s:begin body)                    (for-each loop body)]
      [(s:dispatch-lambda procs)         (for-each loop procs)]
      [(s:if test then else)             (loop test) (loop then) (loop else)]
      [(s:lambda ids rest-id body)
       (for-each reset-lexical! ids)
       (when rest-id (reset-lexical! rest-id))
       (loop body)]
      [(s:let-values ids vals body)
       (for-each (lambda (ids) (for-each reset-lexical! ids)) ids)
       (for-each loop vals)
       (loop body)]
      [(s:fix ids procs body)
       (for-each reset-lexical! ids)
       (for-each loop procs)
       (loop body)]
      [(s:wcm key value expr)            (loop key) (loop value) (loop expr)]
      [_ (error 'contraction "use-count-hashes: not matched" node)])))

(define (global-replacement node global-use-hash)
  (define (global-use-count id) (hash-ref global-use-hash id))
  (match node
    [(s:program body)
     (foldl (lambda (node hash)
              (match node
                [(s:define-values (list id) value)
                 (if (and (= 1 (global-use-count id))
                          (not (inlineable? value))
                          (id-definition-can-be-dropped? id))
                     (hash-set hash id value)
                     hash)]
                [_ hash]))
            (make-immutable-hasheq '())
            body)]
    [_ (error "contraction global-replacement: not matched" node)]))

(define id-definition-can-be-dropped?
  (let ([not-droppable (map sines-variable-id
                            (list
                             l:continuation-index
                             l:continuation-state
                             l:current-toplevel
                             l:stack-traces
                             l:tail-object
                             l:store-continuation-exception
                             l:switch-continuation-exception
                             l:tail-trampoline
                             l:continuation-trampoline
                             l:tail-trampoline-restart
                             l:error
                             l:list
                             l:arguments->rest-parameter
                             l:wrap-for-callback
                             ))])
    (lambda (id)
      (not (member id not-droppable)))))

(define (lookup-env env id)
  (hash-ref env id (lambda () #f)))

(define (extend-env env id value)
  (hash-set env id value))

(define (contraction node lexical-use-hash global-use-hash env global-env)
  (define (global-used? id) (not (zero? (hash-ref global-use-hash id))))

  (let loop ([node node] [env env] [global-env global-env])
    (define (list-contraction nodes env global-env)
      (map (lambda (a) (loop a env global-env))
           nodes))
    (match node
      [(s:literal) node]
      [(s:primapp transformer args loc) (apply-constant-folder (s:primapp transformer (list-contraction args env global-env) loc))]
      [(s:lambda ids rest-id body loc)  (s:lambda ids rest-id (loop body env global-env) loc)]
      [(s:dispatch-lambda procs loc)    (s:dispatch-lambda (list-contraction procs env global-env) loc)]
      [(s:wcm key value expr loc)       (s:wcm (loop key env global-env) (loop value env global-env) (loop expr env global-env) loc)]
      [(s:loop ids vals body loc)       (s:loop ids (list-contraction vals env global-env) (loop body env global-env) loc)]
      [(s:iterate args loc)             (s:iterate (list-contraction args env global-env) loc)]
      [(s:lexical-ref id)
       (cond [(lookup-env env id) => (lambda (node) (loop node env global-env))]
             [else node])]
      [(s:global-ref id)
       (cond [(lookup-env global-env id) => (lambda (node) (loop node env global-env))]
             [else node])]
      [(s:app op args loc)
       (if (or (lambda-stx? op) (dispatch-lambda-stx? op))
           (loop (app->let-node loc op args) env global-env)
           (apply-constant-folder (s:app (loop op env global-env) (list-contraction args env global-env) loc)))]
      [(s:begin body loc)
       (match (list-contraction body env global-env)
         [(list forms ... last-form)
          (let ([forms (filter may-have-side-effects? forms)])
            (if (null? forms)
                last-form
                (s:begin (append forms (list last-form)) loc)))])]
      [(s:define-values ids value loc)
       (cond [(or (null? ids)
                  (and (null? (cdr ids))
                       (not (global-used? (car ids)))
                       (id-definition-can-be-dropped? (car ids))))
              (if (may-have-side-effects? value)
                  (loop value env global-env)
                  (s:literal 42))]
             [(and (null? (cdr ids)) (lookup-env global-env (car ids)))
              (s:literal 42)]
             [else (s:define-values ids (loop value env global-env) loc)])]
      [(s:if test then else loc)
       (match (loop test env global-env)
         [(s:literal value)
          (loop (if value then else) env global-env)]
         [test (s:if test (loop then env global-env) (loop else env global-env) loc)])]
      [(s:let-values ids vals body loc)
       (let-values ([(ids vals new-env not-used) (env-and-bindings-from-let lexical-use-hash ids (list-contraction vals env global-env) env body)])
        (let ([new-node (let ([new-body (loop body new-env global-env)])
                          (if (null? ids) new-body (s:let-values ids vals new-body loc)))])
          (if (null? not-used) new-node (s:begin (append not-used (list new-node))))))]
      [(s:fix ids procs body loc)
       (let-values ([(ids procs new-env not-used) (env-and-bindings-from-fix lexical-use-hash ids procs env)])
         (let ([new-node (let ([new-body (loop body new-env global-env)])
                           (if (null? ids) new-body (s:fix ids (list-contraction procs new-env global-env) new-body loc)))])
           (if (null? not-used) new-node (s:begin (append not-used (list new-node))))))]
      [(s:program body loc)
       (s:program (filter may-have-side-effects? (list-contraction body env global-env)) loc)]
      [_ (error "contraction: not matched" node)])))

(define (env-and-bindings-from-let lexical-use-hash ids vals env body)
  (define (lexical-use-count id) (hash-ref lexical-use-hash id))
  (let loop ([ids ids] [vals vals] [env env] [bindings '()] [not-used '()])
    (match ids
      [(list)
       (let ([bindings (reverse bindings)])
         (values (map car bindings) (map cdr bindings) env (reverse not-used)))]
      [(list (list id) idss ...)
       (match vals
         [(list val vals ...)
          (cond [(zero? (lexical-use-count id))
                 (loop idss vals env bindings
                     (if (may-have-side-effects? val)
                         (cons val not-used)
                         not-used))]
                [(or (literal-stx? val)
                     (lexical-ref-stx? val)
                     (global-ref-stx? val)
                     (and (= 1 (lexical-use-count id))
                          (or (referencial-transparent? val)
                              (and (not (may-have-side-effects? val))
                                   (lexical-id-used-in-head? id body)
                                   (no-side-effects-on-head? body)))))
                 (loop idss vals (extend-env env id val) bindings not-used)]
                [else (loop idss vals env (cons (cons (list id) val) bindings) not-used)])])]
      [(list (list ids ...) idss ...)
       (match vals
         [(list val vals ...)
          (loop idss vals env (cons (cons ids val) bindings) not-used)])])))

(define (env-and-bindings-from-fix lexical-use-hash ids procs env)
  (define (lexical-use-count id) (hash-ref lexical-use-hash id))
  (let loop ([ids ids] [vals procs] [env env] [bindings '()] [not-used '()])
    (if (null? ids)
        (let ([bindings (reverse bindings)])
          (values (map car bindings) (map cdr bindings) env (reverse not-used)))
        (let ([id (car ids)]
              [ids (cdr ids)])
          (match vals
            [(list val vals ...)
             (cond [(zero? (lexical-use-count id))
                    (loop ids vals env bindings not-used)]
                   [(= 1 (lexical-use-count id))
                    (loop ids vals (extend-env env id val) bindings not-used)]
                   [else
                    (loop ids vals env (cons (cons id val) bindings) not-used)])])))))

(define (lexical-id-used-in-head? id body)
  (match body
    [(or (s:literal) (s:global-ref) (s:dispatch-lambda) (s:lambda) (s:let-values) (s:fix) (s:wcm) (s:loop)) #f]
    [(s:lexical-ref id2)  (equal? id id2)]
    [(s:primapp op args) (ormap (lambda (arg) (lexical-id-used-in-head? id arg)) args)]
    [(s:app op args)     (or (lexical-id-used-in-head? id op) (ormap (lambda (arg) (lexical-id-used-in-head? id arg)) args))]
    [(s:begin body)      (lexical-id-used-in-head? id (car body))]
    [(s:if test _ _)     (lexical-id-used-in-head? id test)]
    [_ (error "contraction lexical-id-used-in-head?: not matched" body)]))

(define (no-side-effects-on-head? body)
  (define (no-side-effects? body)
    (match body
      [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:dispatch-lambda) (s:lambda) (s:app))
       #t]
      [(or (s:let-values) (s:fix) (s:wcm))
       #f]
      [(s:primapp op args)
       (and (sines-primitive-side-effects-free? op)
            (andmap no-side-effects? args))]
      [(s:begin body)
       (no-side-effects-on-head? (car body))]
      [(s:if test _ _)
       (no-side-effects-on-head? test)]
      [_ (error "contraction no-side-effects?: not matched" body)]))
  (match body
    [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:dispatch-lambda) (s:lambda))
     #t]
    [(or (s:let-values) (s:fix) (s:wcm))
     #f]
    [(s:primapp op args)
     (and (sines-primitive-side-effects-free? op)
          (andmap no-side-effects? args))]
    [(s:app op args)
     (and (no-side-effects? op) (ormap no-side-effects? args))]
    [(s:begin body)
     (no-side-effects-on-head? (car body))]
    [(s:if test _ _)
     (no-side-effects-on-head? test)]
    [_ (error "contraction no-side-effects-on-head?: not matched" body)]))

(define (referencial-transparent? node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:lambda) (s:dispatch-lambda))
     #t]
    [_ #f] ;; HACK
    [(or (s:define-values) (s:app) (s:wcm))
     #f]
    [(s:primapp op args)
     (and (sines-primitive-referencial-transparent? op)
          (andmap referencial-transparent? args))]
    [(s:begin body)
     (andmap referencial-transparent? body)]
    [(s:if test then else)
     (and (referencial-transparent? test)
          (referencial-transparent? then)
          (referencial-transparent? else))]
    [(s:let-values _ vals body)
     (and (andmap referencial-transparent? vals)
          (referencial-transparent? body))]
    [(s:fix _ _ body)
     (referencial-transparent? body)]
    [_ (error "contraction side-effects-free?: not matched" node)]))

(define (may-have-side-effects? node)
  (not (side-effects-free? node)))

(define (side-effects-free? node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:lambda) (s:dispatch-lambda))
     #t]
    [(or (s:define-values) (s:app) (s:wcm) (s:loop))
     #f]
    [(s:primapp op args)          (and (sines-primitive-side-effects-free? op) (andmap side-effects-free? args))]
    [(s:begin body)               (andmap side-effects-free? body)]
    [(s:if test then else)        (and (side-effects-free? test) (side-effects-free? then) (side-effects-free? else))]
    [(s:let-values _ vals body)   (and (andmap side-effects-free? vals) (side-effects-free? body))]
    [(s:fix _ _ body)             (side-effects-free? body)]
    [_ (error "contraction side-effects-free?: not matched" node)]))

(define (inlineable? node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:lambda) (s:dispatch-lambda))
     #f]
    [(or (s:app) (s:wcm) (s:let-values)) ;; Sometimes safe
     #t]
    [(s:primapp op args)
     (or (not (sines-primitive-side-effects-free? op))
         (ormap inlineable? args))]
    [(s:begin body) (ormap inlineable? body)]
    [(s:if test then else) (or (inlineable? test) (inlineable? then) (inlineable? else))]
    [(s:fix _ _ body) (inlineable? body)]
    [_ (error "contraction inlineable?: not matched" node)]))

