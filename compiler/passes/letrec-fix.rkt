#lang scheme/base

;; This pass was extracted from the paper Fixing Letrec by Waddell and Dybvig

(require
 scheme/base
 scheme/contract
 scheme/list
 scheme/match
 scheme/dict
 syntax/id-table
 (prefix-in p: "../primitives2.rkt")
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.rkt")

(define (transform node)
  (let-values ([(use-count mutation-count) (use+mutation-count-hashes node)])
    (letrec-fix node use-count mutation-count)))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (use+mutation-count-hashes node)
  (let ([use-hash (make-hasheq)]
        [mutation-hash (make-hasheq)])
    (define (reset! id)
      (cond [(hash-ref use-hash id (lambda () #f)) => (lambda (v) (error 'letrec-fix "use+mutation-count-hashes: lexical id already in hash ~A" id))])
      (hash-set! use-hash id 0)
      (hash-set! mutation-hash id 0))
    (define (inc-use! id)
      (hash-set! use-hash id (add1 (hash-ref use-hash id))))
    (define (inc-mutation! id)
      (hash-set! mutation-hash id (add1 (hash-ref mutation-hash id))))
    (let loop ([node node])
      (match node
        [(or (s:literal) (s:global-ref))   (void)]
        [(s:lexical-ref id)                (inc-use! id)]
        [(s:primapp _ args)                (for-each loop args)]
        [(s:app op args)                   (loop op) (for-each loop args)]
        [(s:begin body)                    (for-each loop body)]
        [(s:dispatch-lambda procs)         (for-each loop procs)]
        [(s:define-values _ value)         (loop value)]
        [(s:if test then else)             (loop test) (loop then) (loop else)]
        [(s:lambda ids rest-id body)
         (for-each reset! ids)
         (when rest-id (reset! rest-id))
         (loop body)]
        [(s:let-values ids values body)
         (for-each (lambda (ids) (for-each reset! ids)) ids)
         (for-each loop values)
         (loop body)]
        [(s:letrec-values ids values body)
         (for-each (lambda (ids) (for-each reset! ids)) ids)
         (for-each loop values)
         (loop body)]
        [(s:set! var value)
         (when (lexical-ref-stx? var)
           (inc-mutation! (lexical-ref-stx-id var)))
         (loop value)]
        [(s:wcm key value expr) (loop key) (loop value) (loop expr)]
        [(s:program body)       (map loop body)]
        [_ (error 'letrec-fix "use+mutation-count-hashes: not matched, in ~A" node)]))
    (values use-hash mutation-hash)))

(define (letrec-fix node use-hash mutation-hash)
  ;; FIXME
  ;; ;; This is valid for letrec; it cannot be used for now because inner definitions are translated to letrec and those must be ordered
  ;; (define (unreferenced? id)
  ;;   (hash-ref use-hash id (lambda () #f)))
  (define (unreferenced? id)
    #f)

  (define (use-count id)
    (hash-ref use-hash id))

  ;; A simple expression contains no occurences of the variables bound by the letrec
  ;; expression and must not be able to obtain its continuation via call/cc, either directly
  ;; or indirectly
  (define (simple? ids value id-list)
    (define (simple? node id-list)
      (match node
        [(or (s:literal) (s:global-ref)) #t]
        [(s:lexical-ref id)    (not (memq id id-list))]
        ;; TODO: side-effect free primitives are simple
        [(s:begin body)        (andmap (lambda (node) (simple? node id-list)) body)]
        [(s:if test then else) (and (simple? test id-list) (simple? then id-list) (simple? else id-list))]
        [_ #f]))
    (and (zero? (foldr + 0 (map (lambda (id) (hash-ref mutation-hash id)) ids)))
         (simple? value id-list)))

  (define (proc? id value)
    (and (zero? (hash-ref mutation-hash id))
         (or (lambda-stx? value)
             (dispatch-lambda-stx? value))))

  (let loop ([node node])
    (match node
      [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
      [(s:primapp transformer args loc)   (s:primapp transformer (map loop args) loc)]
      [(s:app op args loc)                (s:app (loop op) (map loop args) loc)]
      [(s:begin body loc)                 (s:begin (map loop body) loc)]
      [(s:lambda ids rest-id body loc)    (s:lambda ids rest-id (loop body) loc)]
      [(s:dispatch-lambda procs loc)      (s:dispatch-lambda (map loop procs) loc)]
      [(s:if test then else loc)          (s:if (loop test) (loop then) (loop else) loc)]
      [(s:let-values ids vals body loc)   (s:let-values ids (map loop vals) (loop body) loc)]
      [(s:define-values ids value loc)    (s:define-values ids (loop value) loc)]
      [(s:wcm key value expr loc)         (s:wcm (loop key) (loop value) (loop expr) loc)]
      [(s:program body loc)               (s:program (map loop body) loc)]
      [(s:letrec-values idss vals body loc)
       (let-values ([(unreferenced other) (partition (lambda (p)
                                                       (let ([ids (car p)])
                                                         (or (null? ids)
                                                             (andmap unreferenced? ids))))
                                                     (map cons idss (map loop vals)))])
         (let-values ([(simple other) (partition (let ([all-ids (apply append idss)])
                                                   (lambda (p)
                                                     (let ([ids (car p)])
                                                       (and (null? (cdr ids))
                                                            (simple? ids (cdr p) all-ids)))))
                                                 other)])
           (let-values ([(proc complex) (partition (lambda (p)
                                                     (let ([ids (car p)])
                                                       (and (null? (cdr ids))
                                                            (proc? (car ids) (cdr p)))))
                                                   other)])
             (let-values ([(complex/single complex/multiple) (partition (lambda (p)
                                                                          (let ([ids (car p)]) (null? (cdr ids))))
                                                                        complex)])
               (let ([new-ids (map (lambda (_) (new-lexical-id)) complex/multiple)])
                 (s:let-values (append (map (lambda (p) (list (caar p))) simple)
                                       (map (lambda (id) (list id)) (append new-ids (apply append (map car complex)))))
                               (append (map cdr simple)
                                       (map (lambda (_) (s:primapp p:%%void '()))
                                            (append new-ids (apply append (map car complex)))))
                               (s:fix (map caar proc) (map cdr proc)
                                      (s:begin (append (map (lambda (pair) (s:set! (s:lexical-ref (caar pair)) (cdr pair)))
                                                            complex/single)
                                                       (append-map (lambda (new-id pair)
                                                                     (cons (s:set! (s:lexical-ref new-id) (cdr pair))
                                                                           (for/list ([id (in-list (car pair))]
                                                                                      [i (in-range (length (car pair)))])
                                                                             (s:set! (s:lexical-ref id) (s:primapp p:%%from-values (list (s:lexical-ref new-id) (s:literal i)))))))
                                                                   new-ids complex/multiple)
                                                       (map cdr unreferenced)
                                                       (list (loop body)))
                                               loc)
                                      loc)
                               loc))))))]
      [(s:set! var value loc)
       (if (and (lexical-ref-stx? var) (zero? (use-count (lexical-ref-stx-id var))))
           (loop value)
           (s:set! (loop var) (loop value) loc))]
      [_ (error 'letrec-fix "not matched ~A" node)])))
