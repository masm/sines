#lang scheme

(require
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.rkt")

(define (transform node)
  (loopify node))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (loopify node)
  (match node
    [(or (s:literal) (s:global-ref) (s:lexical-ref)) node]
    [(s:primapp transformer args loc)   (s:primapp transformer (map loopify args) loc)]
    [(s:app op args loc)                (s:app (loopify op) (map loopify args) loc)]
    [(s:begin body loc)                 (s:begin (map loopify body) loc)]
    [(s:define-values ids value loc)    (s:define-values ids (loopify value) loc)]
    [(s:if test then else loc)          (s:if (loopify test) (loopify then) (loopify else) loc)]
    [(s:lambda ids rest-id body loc)    (s:lambda ids rest-id (loopify body) loc)]
    [(s:dispatch-lambda procs loc)      (s:dispatch-lambda (map loopify procs) loc)]
    [(s:let-values ids vals body loc)   (s:let-values ids (map loopify vals) (loopify body) loc)]
    [(s:fix ids procs body loc)
     (let ([new-procs (map (lambda (id proc)
                             (match proc
                               [(s:lambda ids rest-id body)
                                (if (and (not rest-id) (simple-and-only-tail-calls-itself? id body))
                                    (replace ids body)
                                    proc)]
                               [(s:dispatch-lambda) proc]))
                           ids (map loopify procs))])
       (s:fix ids new-procs (loopify body) loc))]
    [(s:wcm key value expr loc)         (s:wcm (loopify key) (loopify value) (loopify expr) loc)]
    [(s:program body loc)               (s:program (map loopify body) loc)]
    [_ (error 'loopify "not matched" node)]))

(define (simple-and-only-tail-calls-itself? id body)
  (let loop ([node body] [tail? #t])
    (match node
      [(or (s:literal) (s:global-ref) (s:lexical-ref)) #t]
      [(s:primapp _ args)           (andmap (lambda (n) (loop n #f)) args)]
      [(s:app op args)
       (and tail?
            (lexical-ref-stx? op)
            (equal? id (lexical-ref-stx-id op))
            (andmap (lambda (n) (loop n #f)) args))]
      [(s:begin (list init ... last))
       (and (andmap (lambda (n) (loop n #f)) init)
            (loop last tail?))]
      [(s:if test then else)        (and (loop test #f) (loop then tail?) (loop else tail?))]
      [(s:lambda)                   #f]
      [(s:dispatch-lambda)          #f]
      [(s:let-values _ vals body)   (and (andmap (lambda (n) (loop n #f)) vals) (loop body tail?))]
      [(s:fix ids procs body)       #f]
      [(s:wcm key value expr)       #f]
      [_ (error 'loopify "simple-and-only-tail-calls-itself?: not matched" node)])))

(define (replace ids node)
  (let ([new-ids (map lexical-id-clone ids)])
    (s:lambda new-ids #f (s:loop ids (map (lambda (id) (make-lexical-ref-stx #f id)) new-ids) (replace-app-by-iterate node)))))

(define (replace-app-by-iterate node)
  (let loop ([node node])
    (match node
      [(or (s:literal) (s:global-ref) (s:lexical-ref) (s:primapp)) node]
      [(s:app _ args loc)                 (s:iterate args loc)]
      [(s:begin (list init ... last) loc) (s:begin (append init (list (loop last))) loc)]
      [(s:if test then else loc)          (s:if test (loop then) (loop else) loc)]
      [(s:let-values ids vals body loc)   (s:let-values ids vals (loop body) loc)]
      [_ (error 'loopify "replace-app-by-iterate: not matched" node)])))
