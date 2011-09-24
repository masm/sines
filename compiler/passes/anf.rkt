#lang scheme

(require
 "../primitives.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.rkt")

(define (transform node)
  (normalize-term (alpha-renamed-lexicals #:full? #t node)))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (normalize-term node)
  (normalize node (lambda (x) x)))

(define (normalize node k)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) (k node)]
    [(s:primapp transformer args loc)
     (normalize-names args (lambda (t*) (k (s:primapp transformer t* loc))))]
    [(s:app op args loc)
     (normalize-name op (lambda (t)
                          (normalize-names args (lambda (t*)
                                                  (k (s:app t t* loc))))))]
    [(s:begin body loc) (k (s:begin (map normalize-term body) loc))]
    [(s:define-values ids value loc)
     (normalize value (lambda (t)
                        (k (s:define-values ids t loc))))]
    [(s:if test then else loc)
     (normalize-if-test test (lambda (t)
                               (k (s:if t
                                        (normalize-term then)
                                        (normalize-term else) loc))))]
    [(s:lambda params rest-param body loc) (k (s:lambda params rest-param (normalize-term body) loc))]
    [(s:dispatch-lambda procs loc) (k (s:dispatch-lambda (map normalize-term procs) loc))]
    [(s:let-values ids vals body loc)
     (if (null? vals)
         (normalize body k)
         (normalize (car vals)
                    (lambda (t)
                      (s:let-values (list (car ids)) (list t)
                                    (normalize (s:let-values (cdr ids) (cdr vals) body)
                                               k) loc))))]
    [(s:fix ids vals body loc)
     (s:fix ids
            (map normalize-term vals)
            (normalize body k) loc)]
    [(s:loop ids vals body loc)
     (normalize-names vals (lambda (t*) (k (s:loop ids t* body loc))))]
    [(s:program body loc) (k (s:program (map normalize-term body) loc))]
    [_ (error "anf normalize: not matched" node)]))

(define (normalize-name node k)
  (normalize node (lambda (new-node)
                    (if (or (literal-stx? new-node)
                            (lexical-ref-stx? new-node)
                            (global-ref-stx? new-node)
                            (and (primapp-stx? new-node)
                                 (sines-primitive-side-effects-free? (primapp-stx-transformer new-node)))
                            (lambda-stx? new-node))
                        (k new-node)
                        (let ([t (new-lexical-id)])
                          (s:let-values (list (list t))
                                        (list new-node)
                                        (k (s:lexical-ref t))))))))

(define (normalize-names nodes k)
  (if (null? nodes)
      (k '())
      (normalize-name (car nodes)
                      (lambda (t)
                        (normalize-names (cdr nodes)
                                          (lambda (t*)
                                            (k (cons t t*))))))))

;; The following is needed because we must not have primapp in if tests
;; in the labels-bodies pass because the way ifs are flattened
(define (normalize-if-test node k)
  (normalize node (lambda (new-node)
                    (if (or (literal-stx? new-node)
                            (lexical-ref-stx? new-node)
                            (global-ref-stx? new-node))
                        (k new-node)
                        (let ([t (new-lexical-id)])
                          (s:let-values (list (list t))
                                        (list new-node)
                                        (k (s:lexical-ref t))))))))
