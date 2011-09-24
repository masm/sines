#lang scheme

(require
 (prefix-in p: "../primitives2.ss")
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.rkt")

(define (transform node)
  (convert node))

(provide/contract
 [transform (stx? . -> . stx?)])

;;;

(define id-list (make-parameter '()))

(define (add-id id)
  (id-list (cons id (id-list))))

(define (convert node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:primapp) (s:app)
         (s:if) (s:lambda) (s:dispatch-lambda) (s:lexical-init) (s:global-init))
     node]
    [(s:begin body loc) (s:begin (map convert body) loc)]
    [(s:define-values ids val loc)
     (let-values ([(init-id init-nodes)
                   (cond [(null? ids)       (values #f (list val))]
                         [(null? (cdr ids)) (values #f (list (s:global-init (car ids) val loc)))]
                         [else
                          (let ([new-id (new-global-id)])
                            (values new-id
                                    (cons (s:global-init new-id val loc)
                                          (for/list ([id (in-list ids)]
                                                     [i (in-range (length ids))])
                                            (s:global-init id (s:primapp p:%%from-values (list (s:global-ref new-id) (s:literal i))))))))])])
       (let ([new-ids (if init-id (cons init-id ids) ids) ])
         (for-each add-id new-ids)
         (if (null? (cdr init-nodes))
             (car init-nodes)
             (s:begin init-nodes loc))))]
    [(s:program/var global-ids lexical-ids body loc)
     (parameterize ([id-list '()])
       (let ([new-body (map convert body)])
         (s:program/var (append global-ids (id-list)) lexical-ids new-body loc)))]
    [_ (error "define-conversion: not matched" node)]))


