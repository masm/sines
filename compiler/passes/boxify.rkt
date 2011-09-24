#lang scheme/base

(require
 scheme/base
 scheme/contract
 scheme/list
 scheme/match
 (prefix-in p: "../primitives.rkt")
 (prefix-in p: "../primitives2.rkt")
 "../store.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.rkt")

(define (transform node)
  (let-values ([(lexical-use-hash lexical-mutation-hash global-use-hash global-mutation-hash) (use+mutation-count-hashes node)])
    (boxify node lexical-use-hash lexical-mutation-hash global-use-hash global-mutation-hash)))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (use+mutation-count-hashes node)
  (let ([lexical-use-hash (make-hasheq)]
        [lexical-mutation-hash (make-hasheq)]
        [global-use-hash (make-hasheq)]
        [global-mutation-hash (make-hasheq)])
    (define (reset-lexical! id)
      (cond [(hash-ref lexical-use-hash id (lambda () #f)) => (lambda (v) (error 'boxify "use+mutation-count-hashes: lexical id already in hash ~A" id))])
      (hash-set! lexical-use-hash id 0)
      (hash-set! lexical-mutation-hash id 0))
    (define (inc-lexical-use! id)
      (hash-set! lexical-use-hash id (add1 (hash-ref lexical-use-hash id))))
    (define (inc-lexical-mutation! id)
      (hash-set! lexical-mutation-hash id (add1 (hash-ref lexical-mutation-hash id))))
    (define (reset-global! id)
      (cond [(hash-ref global-use-hash id (lambda () #f)) => (lambda (v) (error 'boxify "use+mutation-count-hashes: global id already in hash ~A" id))])
      (hash-set! global-use-hash id 0)
      (hash-set! global-mutation-hash id 0))
    (define (inc-global-use! id)
      (hash-set! global-use-hash id (add1 (hash-ref global-use-hash id
                                                    (lambda () (error 'boxify "use+mutation-count-hashes: global id not in use hash ~A" id))))))
    (define (inc-global-mutation! id)
      (hash-set! global-mutation-hash id (add1 (hash-ref global-mutation-hash id
                                                         (lambda () (error 'boxify "use+mutation-count-hashes: global id not in mutation hash ~A" id))))))
    (let loop ([node node])
      (match node
        [(struct literal-stx _) (void)]
        [(s:global-ref id)                              (inc-global-use! id)]
        [(struct lexical-ref-stx [_ id])                (inc-lexical-use! id)]
        [(struct primapp-stx [_ _ args])                (for-each loop args)]
        [(struct app-stx [_ op args])                   (loop op) (for-each loop args)]
        [(struct begin-stx [_ body])                    (for-each loop body)]
        [(struct dispatch-lambda-stx [_ procs])         (for-each loop procs)]
        [(struct define-values-stx [_ _ value])         (loop value)] ; reset-global! was called in the program node
        [(struct if-stx [_ test then else])             (loop test) (loop then) (loop else)]
        [(struct lambda-stx [_ ids rest-id body])
         (for-each reset-lexical! ids)
         (when rest-id (reset-lexical! rest-id))
         (loop body)]
        [(struct let-values-stx [_ ids values body])
         (for-each (lambda (ids) (for-each reset-lexical! ids)) ids)
         (for-each loop values)
         (loop body)]
        [(struct fix-stx [_ ids procs body])
         (for-each reset-lexical! ids)
         (for-each loop procs)
         (loop body)]
        [(struct set!-stx [_ var value])
         (match var
           [(s:lexical-ref id) (inc-lexical-mutation! id)]
           [(s:global-ref id) (inc-global-mutation! id)])
         (loop value)]
        [(struct wcm-stx [_ key value expr]) (loop key) (loop value) (loop expr)]
        [(struct program-stx [_ body])
         (for-each (lambda (n)
                     (match n
                       [(s:define-values ids _) (for-each reset-global! ids)]
                       [_ (void)]))
                   body)
         (map loop body)]
        [_ (error 'boxify "use+mutation-count-hashes: not matched" node)]))
    (values lexical-use-hash lexical-mutation-hash global-use-hash global-mutation-hash)))

(define (boxify node lexical-use-hash lexical-mutation-hash global-use-hash global-mutation-hash)
  (define (lexical-mutated? id)
    (not (zero? (hash-ref lexical-mutation-hash id))))
  (define (global-mutated? id)
    (not (zero? (hash-ref global-mutation-hash id))))
  (define (boxify node)
    (s:primapp p:%%box (list node)))
  (define (unboxify node)
    (s:primapp p:%%unbox (list node)))
  (let loop ([node node])
    (match node
      [(s:literal) node]
      [(s:lexical-ref id) (if (lexical-mutated? id) (unboxify node) node)]
      [(s:global-ref id) (if (global-mutated? id) (unboxify node) node)]
      [(s:primapp primitive args loc) (s:primapp primitive (map loop args) loc)]
      [(s:app op args loc) (s:app (loop op) (map loop args) loc)]
      [(s:begin body loc) (s:begin (map loop body) loc)]
      [(s:define-values ids value loc)
       (s:define-values ids (if (global-mutated? (car ids))
                                (boxify (loop value))
                                (loop value))
                        loc)]
      [(s:if test then else loc) (s:if (loop test) (loop then) (loop else) loc)]
      [(s:lambda ids rest-id body loc)
       (let ([all-ids (if rest-id (cons rest-id ids) ids)])
         (if (ormap lexical-mutated? all-ids)
             (let ([ids-and-pairs (map (lambda (id)
                                         (if (lexical-mutated? id)
                                             (cons id (lexical-id-clone id))
                                             id))
                                       all-ids)])
               (let ([new-ids (map (lambda (id-or-pair)
                                     (if (pair? id-or-pair) (cdr id-or-pair) id-or-pair))
                                   ids-and-pairs)]
                     [pairs (filter pair? ids-and-pairs)])
                 (s:lambda (if rest-id (cdr new-ids) new-ids)
                           (and rest-id (car new-ids))
                           (s:let-values (map (lambda (pair) (list (car pair)))
                                              pairs)
                                         (map (lambda (pair)
                                                (boxify (s:lexical-ref (cdr pair))))
                                              pairs)
                                         (loop body)
                                         loc)
                           loc)))
             (s:lambda ids rest-id (loop body) loc)))]
      [(s:dispatch-lambda procs loc) (s:dispatch-lambda (map loop procs) loc)]
      [(s:let-values idss vals body loc)
       (let ([alist (map (lambda (id) (cons id (lexical-id-clone id)))
                         (apply append (map (lambda (ids) (filter lexical-mutated? ids))
                                            (filter (lambda (ids) (< 1 (length ids))) idss))))])
         (s:let-values (map (lambda (ids)
                              (map (lambda (id) (cond [(assoc id alist) => cdr] [else id])) ids))
                            idss)
                       (map (lambda (ids val)
                              (let ([val (loop val)])
                                (if (and (= 1 (length ids)) (lexical-mutated? (car ids)))
                                    (boxify val)
                                    val)))
                            idss vals)
                       (let ([body (loop body)])
                         (if (null? alist)
                             body
                             (s:let-values (map list (map car alist)) (map (lambda (a) (boxify (s:lexical-ref a))) (map cdr alist)) body)))
                       loc))]
      [(s:fix ids procs body loc) (s:fix ids (map loop procs) (loop body) loc)]
      [(s:set! var val loc) (s:primapp p:%%set-box! (list var (loop val)) loc)]
      [(s:wcm key val expr loc) (s:wcm (loop key) (loop val) (loop expr) loc)]
      [(s:program body loc) (s:program (map loop body) loc)])))
