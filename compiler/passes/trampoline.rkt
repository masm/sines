#lang scheme

(require
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt"))

(define (transform node)
  (trampoline node))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (use-count-hashes node)
  (let ([lexicals (make-hasheq)]
        [globals (make-hash)]
        [lexicals-body (make-hasheq)]
        [globals-body (make-hash)]
        [lexicals-tbody (make-hasheq)]
        [globals-tbody (make-hash)])
    (let loop ([node node] [recursive '()])
      (define (map-loop nodes recursive)
        (map (lambda (x) (loop n recursive)) nodes))
      (define (lexical-is-tail-recursive? id)
        (cond [(hash-key? lexicals id)
               (hash-ref lexicals id)]
              [else
               (let ([new-body (loop (hash-ref lexicals-body id) )]))]))

      (match node
        [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
        [(s:primapp transformer args loc) (s:primapp transformer (map-loop args recursive) loc)]
        [(s:app op args loc)              (s:app (loop op recursive) (map-loop args recursive) loc)]
        [(s:begin body loc)               (s:begin (map-loop body recursive) loc)]
        [(s:begin/var ids body loc)       (s:begin/var ids (map-loop body recursive) loc)]
        [(s:if test then else loc)        (s:if (loop test recursive) (loop then recursive) (loop else recursive) loc)]
        [(s:lambda ids rest-id body loc)  (s:lambda ids rest-id (loop body recursive) loc)]
        [(s:dispatch-lambda procs loc)    (s:dispatch-lambda (map-loop procs recursive) loc)]
        [(s:lexical-init id value loc)    (s:lexical-init id (loop value recursive) loc)]
        [(s:global-init id value loc)     (s:global-init id (loop value recursive) loc)]
        [(s:loop ids values body loc)     (s:loop ids (map-loop values recursive) (loop body recursive) loc)]
        [(s:iterate args loc)             (s:iterate (map-loop args recursive) loc)]
        [(s:tail node loc)
         (match node
           [(s:app op args loc2)
            (match op
              [(s:lexical-ref id)
               (if lexical)])]
           [_ (s:tail (loop node recursive) loc)])]
        [(s:program/var global-ids lexical-ids body loc) (s:program/var global-ids lexical-ids (map-loop body recursive) loc)]
        [_ (error 'tail-form "loop: not matched ~A" node)]))))

