#lang racket/base

(require
 racket/base
 racket/contract
 racket/list
 racket/match
 "../backend.rkt"
 "../constant-folding-defs.rkt"
 "../primitives.rkt"
 (prefix-in p: "../primitives2.rkt")
 "../syntax.rkt"
 "../store.rkt"
 "../utils.rkt"
 (prefix-in s: "../syntax2.rkt")
 (prefix-in l: "../../private/library.ss"))

(define (transform node)
  (let-values ([(lexical-use-hash global-use-hash) (use-count-hashes node)])
    (update-global-use-hash-for-non-droppable-definitions! global-use-hash)
    (contraction node lexical-use-hash global-use-hash)))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (update-global-use-hash-for-non-droppable-definitions! hash)
  (for-each (lambda (v)
              (hash-set! hash (sines-variable-id v) 42))
            ;; The definitions of the following will no be dropped.
            (list l:continuation-index
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
                  l:wrap-for-callback))
  hash)

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

(define (update-use-count-hashes! lexical-use-hash global-use-hash node [proc sub1])
  (define (update-lexical-use! id)
    (hash-set! lexical-use-hash id (proc (hash-ref lexical-use-hash id (lambda () 0)))))
  (define (update-global-use! id)
    (hash-set! global-use-hash id (proc (hash-ref global-use-hash id (lambda () 0)))))
  (let loop ([node node])
    (match node
      [(s:literal)                  (void)]
      [(s:global-ref id)            (update-global-use! id)]
      [(s:lexical-ref  id)          (update-lexical-use! id)]
      [(s:primapp  _ args)          (for-each loop args)]
      [(s:app op args)              (loop op) (for-each loop args)]
      [(s:begin body)               (for-each loop body)]
      [(s:dispatch-lambda procs)    (for-each loop procs)]
      [(s:if test then else)        (loop test) (loop then) (loop else)]
      [(s:lambda ids rest-id body)  (loop body)]
      [(s:let-values ids vals body) (for-each loop vals) (loop body)]
      [(s:fix ids procs body)       (for-each loop procs) (loop body)]
      [(s:wcm key value expr)       (loop key) (loop value) (loop expr)]
      [_ (error 'contraction "use-count-hashes: not matched, in ~A" node)])))

(define (lookup-env env id)
  (hash-ref env id (lambda () #f)))

(define (contraction node lexical-use-hash global-use-hash)
  (define (global-use-count  id) (hash-ref global-use-hash id))
  (define (lexical-use-count id) (hash-ref lexical-use-hash id))

  (define (update-use-count! proc vals)
    (for-each (lambda (val) (update-use-count-hashes! lexical-use-hash global-use-hash val proc)) vals))

  (define lexical-env (make-hasheq))
  (define global-env (make-hash))

  (define (extend-lexical-env! idss vals)
    (for-each (lambda (ids val)
                (when (= 1 (length ids))
                  (hash-set! lexical-env (car ids) val)))
              idss vals))

  (let loop ([node node])
    (match node
      [(s:literal) node]
      [(s:primapp transformer args loc) (apply-constant-folder (s:primapp transformer (map loop args) loc))]
      [(s:lambda ids rest-id body loc)  (s:lambda ids rest-id (loop body) loc)]
      [(s:dispatch-lambda procs loc)    (s:dispatch-lambda (map loop procs) loc)]
      [(s:wcm key value expr loc)       (s:wcm (loop key) (loop value) (loop expr) loc)]
      [(s:loop ids vals body loc)       (s:loop ids (map loop vals) (loop body) loc)]
      [(s:iterate args loc)             (s:iterate (map loop args) loc)]
      [(s:lexical-ref id)
       (let ([val (lookup-env lexical-env id)])
         (cond [(and val
                     (or (literal-stx? val) (lexical-ref-stx? val) (global-ref-stx? val)
                         (and (= 1 (lexical-use-count id))
                              (referencial-transparent? val))))
                (update-use-count! sub1 (list node))
                (loop val)]
               [else node]))]
      [(s:global-ref id)
       (cond [(and (= 1 (global-use-count id)) (lookup-env global-env id))
              => (lambda (replacement)
                   (update-use-count! sub1 (list node))
                   (loop replacement))]
             [else node])]
      [(s:app op args loc)
       (if (or (lambda-stx? op) (dispatch-lambda-stx? op))
           (loop (app->let-node loc op args))
           (let ([op (loop op)])
             (if (or (lambda-stx? op) (dispatch-lambda-stx? op))
                 (loop (app->let-node loc op args))
                 (let ([node (s:app op (map loop args) loc)])
                   (let ([new-node (apply-constant-folder node)])
                     (unless (eq? node new-node)
                       (update-use-count! sub1 (list node))
                       (update-use-count! add1 (list new-node)))
                     new-node)))))]
      [(s:begin body loc)
       (match (map loop body)
         [(list forms ... last-form)
          (let-values ([(forms droppable) (partition may-have-side-effects? forms)])
            (update-use-count! sub1 droppable)
            (if (null? forms) last-form (s:begin (append forms (list last-form)) loc)))])]
      [(s:define-values ids value loc)
       (cond [(andmap zero? (map global-use-count ids))
              (if (may-have-side-effects? value)
                  (loop value)
                  (begin0 (s:primapp p:%%void '())
                    (update-use-count! sub1 (list value))))]
             [(and (= 1 (length ids)) (lookup-env global-env (car ids)))
              (update-use-count! sub1 (list value))
              (s:primapp p:%%void '())]
             [else (s:define-values ids (loop value) loc)])]
      [(s:if test then else loc)
       (match (loop test)
         [(s:literal value)
          (cond [value
                 (update-use-count! sub1 (list else))
                 (loop then)]
                [else
                 (update-use-count! sub1 (list then))
                 (loop else)])]
         [test (s:if test (loop then) (loop else) loc)])]
      [(s:let-values idss vals body loc)
       (let-values ([(not-used other) (partition (lambda (p) (andmap zero? (map lexical-use-count (car p))))
                                                 (map cons idss vals))])
         (let-values ([(droppable non-droppable) (partition (lambda (p) (side-effects-free? (cdr p))) not-used)])
           (update-use-count! sub1 (map cdr droppable))
           (extend-lexical-env! (map car other) (map cdr other))
           (let ([new-body (loop body)])
             (let-values ([(not-used2 new-other) (partition (lambda (p) (andmap zero? (map lexical-use-count (car p))))
                                                            other)])
               (let-values ([(droppable2 non-droppable2) (partition (lambda (p) (side-effects-free? (cdr p))) not-used2)])
                 (update-use-count! sub1 (map cdr droppable2))
                 (let ([new-not-used (map loop (map cdr (append non-droppable non-droppable2)))]
                       [newer-body (if (null? new-other) new-body (s:let-values (map car new-other) (map loop (map cdr new-other)) new-body loc))])
                   (if (null? new-not-used) newer-body (s:begin (append new-not-used (list newer-body))))))))))]
      [(s:fix ids procs body loc)
       (let-values ([(not-used other) (partition (lambda (p) (zero? (lexical-use-count (car p))))
                                                 (map cons ids procs))])
         (update-use-count! sub1 (map cdr not-used))
         (let ([uses-before (make-immutable-hasheq (map (lambda (p)
                                                          (let ([id (car p)])
                                                            (cons id (lexical-use-count id))))
                                                        other))])
           (extend-lexical-env! (map list (map car other)) (map cdr other))
           (let ([new-body (loop body)])
             (let ([new-other (filter (lambda (p)
                                        (let ([id (car p)])
                                          (and (not (zero? (lexical-use-count id)))
                                               (not (= 1 (hash-ref uses-before id))))))
                                      other)])
               (let ([new-procs (map (lambda (p)
                                       (let ([id (car p)])
                                         (hash-remove! lexical-env id)
                                         (let ([val (loop (cdr p))])
                                           (hash-set! lexical-env id val)
                                           val)))
                                     new-other)])
                 (begin0 (if (null? new-other) new-body (s:fix (map car new-other) new-procs new-body loc))
                   (let ([recursive (filter (lambda (p)
                                              (let ([id (car p)])
                                                (and (= 1 (hash-ref uses-before id))
                                                     (not (zero? (lexical-use-count id))))))
                                            other)])
                     (update-use-count! sub1 (map cdr recursive))
                     (hash-for-each uses-before
                                    (lambda (id uses)
                                      (when (and (= 1 uses)
                                                 (not (zero? (lexical-use-count id))))
                                        (error 'sanity-check1 "unexpeted ~S, ~S, ~S" (unparse node) id (hash-ref uses-before id))))))))))))]
      [(s:program body loc)
       (for-each (lambda (node)
                   (match node
                     [(s:define-values (list id) val)
                      (when (side-effects-free? val)
                        (hash-set! global-env id val))]
                     [_ (void)]))
                 body)
       (let ([body (map (lambda (node)
                          (match node
                            [(s:define-values) node]
                            [_ (loop node)]))
                        body)])
         (let ([body (map (lambda (node)
                            (match node
                              [(s:define-values) (loop node)]
                              [_ node]))
                          body)])
           (s:program (filter may-have-side-effects? body) loc)))]
      [_ (error 'contraction "not matched, in ~A" node)])))

(require "../parse.ss")

(define (lexical-id-used-in-head? id body)
  (match body
    [(or (s:literal) (s:global-ref) (s:dispatch-lambda) (s:lambda) (s:let-values) (s:fix) (s:wcm) (s:loop)) #f]
    [(s:lexical-ref id2)  (equal? id id2)]
    [(s:primapp op args) (ormap (lambda (arg) (lexical-id-used-in-head? id arg)) args)]
    [(s:app op args)     (or (lexical-id-used-in-head? id op) (ormap (lambda (arg) (lexical-id-used-in-head? id arg)) args))]
    [(s:begin body)      (lexical-id-used-in-head? id (car body))]
    [(s:if test _ _)     (lexical-id-used-in-head? id test)]
    [_ (error 'contraction "lexical-id-used-in-head?: not matched, in ~A" body)]))

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
      [_ (error 'contraction "no-side-effects?: not matched, in ~A" body)]))
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
    [_ (error 'contraction "no-side-effects-on-head?: not matched, in ~A" body)]))

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
    [_ (error 'contraction "referencial-transparent?: not matched, in ~A" node)]))

(define (may-have-side-effects? node)
  (not (side-effects-free? node)))

(define (side-effects-free? node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:lambda) (s:dispatch-lambda)) #t]
    [(or (s:define-values) (s:app) (s:wcm) (s:loop)) #f]
    [(s:primapp op args)          (and (sines-primitive-side-effects-free? op) (andmap side-effects-free? args))]
    [(s:begin body)               (andmap side-effects-free? body)]
    [(s:if test then else)        (and (side-effects-free? test) (side-effects-free? then) (side-effects-free? else))]
    [(s:let-values _ vals body)   (and (andmap side-effects-free? vals) (side-effects-free? body))]
    [(s:fix _ _ body)             (side-effects-free? body)]
    [_ (error 'contraction "side-effects-free?: not matched, in ~A" node)]))
