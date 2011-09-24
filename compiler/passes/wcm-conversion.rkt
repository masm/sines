#lang scheme

(require
 (prefix-in p: "../primitives.rkt")
 (prefix-in p: "../primitives2.rkt")
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.rkt")

(define (transform node)
  (convert node))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define (convert node)
  (let convert ([node node] [tail? #f] [inside-wcm? #f] [var #f])
    (define (list-convert nodes tail? inside-wcm? var)
      (map (lambda (n) (convert n tail? inside-wcm? var)) nodes))
    (match node
      [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:loop)) node]
      [(s:primapp primitive args loc)     (s:primapp primitive (list-convert args #f inside-wcm? var) loc)]
      [(s:app op args loc)
       (s:app (convert op #f inside-wcm? var)
              (cons (if (or tail? (not inside-wcm?))
                        var
                        (s:primapp p:extended-continuation-marks (list var)))
                    (list-convert args #f inside-wcm? var))
              loc)]
      [(s:begin (list init ... last) loc) (s:begin (append (list-convert init #f inside-wcm? var) (list (convert last tail? inside-wcm? var))) loc)]
      [(s:define-values ids value loc)    (s:define-values ids (convert value #f inside-wcm? var) loc)]
      [(s:if test then else loc)          (s:if (convert test #f inside-wcm? var) (convert then tail? inside-wcm? var) (convert else tail? inside-wcm? var) loc)]
      [(s:lambda ids rest-id body loc)
       (let ([id (new-lexical-id)])
         (let ([var (s:lexical-ref id #f)])
           (s:lambda (cons id ids) rest-id (convert body #t #f var) loc)))]
      [(s:dispatch-lambda procs loc)      (s:dispatch-lambda (list-convert procs tail? inside-wcm? var) loc)]
      [(s:let-values ids values body loc) (s:let-values ids (list-convert values #f inside-wcm? var) (convert body tail? inside-wcm? var) loc)]
      [(s:fix ids procs body loc)         (s:fix ids (list-convert procs #f inside-wcm? var) (convert body tail? inside-wcm? var) loc)]
      [(s:wcm key value expr loc)
       (let ([key (convert key #f inside-wcm? var)]
             [value (convert value #f inside-wcm? var)])
         (let ([id (new-lexical-id)])
           (s:let-values (list (list id))
                         (list (s:primapp p:continuation-marks-with-extended-top-frame (list (if tail? var (s:primapp p:extended-continuation-marks (list var))) key value)))
                         (convert expr tail? #t (s:lexical-ref id))
                         loc)))]
      [(s:program body loc)
       (let ([id (new-global-id)])
         (let ([var (s:global-ref id)])
           (s:program (cons (s:define-values (list id) (s:primapp p:initial-mark-frame '()))
                            (list-convert body tail? inside-wcm? var)) loc)))]
      [_ (error "wcp-conversion transform: not matched" node)])))
