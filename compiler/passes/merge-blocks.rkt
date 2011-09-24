#lang scheme

(require
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt"))

(define (transform node)
  (merge-blocks node))

(define (merge-blocks node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref))
     node]
    [(s:primapp primitive args loc)
     (s:primapp primitive (map merge-blocks args) loc)]
    [(s:app op args loc)
     (s:app (merge-blocks op) (map merge-blocks args) loc)]
    [(s:begin body loc)
     (s:begin (map merge-blocks body) loc)]
    [(s:tail app loc)
     (s:tail (merge-blocks app) loc)]
    [(s:if-true test then loc)
     (s:if-true (merge-blocks test) (merge-blocks then) loc)]
    [(s:if test then else loc)
     (s:if (merge-blocks test) (merge-blocks then) (merge-blocks else) loc)]
    [(s:if-false test then loc)
     (s:if-false (merge-blocks test) (merge-blocks then) loc)]
    [(s:lambda ids rest-id
               (s:begin/const/var const-ids const-values ids2 body loc2)
               loc)
     (s:lambda ids rest-id
               (s:begin/const/var const-ids (map merge-blocks const-values) ids2
                                  (make-blocks (append (if rest-id (cons rest-id ids) ids)
                                                       ids2)
                                               (map merge-blocks body))
                                  loc2)
               loc)]
    [(s:dispatch-lambda procs loc)
     (s:dispatch-lambda (map merge-blocks procs) loc)]
    [(s:lexical-init id value loc)
     (s:lexical-init id (merge-blocks value) loc)]
    [(s:global-init id value loc)
     (s:global-init id (merge-blocks value) loc)]
    [(s:loop ids values body loc)
     (s:loop ids (map merge-blocks values) (merge-blocks body) loc)]
    [(s:iterate args loc)
     (s:iterate (map merge-blocks args) loc)]
    [(s:program/const/var const-global-ids const-global-values
                          const-lexical-ids const-lexical-values
                          global-ids lexical-ids body loc)
     (s:program/const/var const-global-ids (map merge-blocks const-global-values)
                          const-lexical-ids (map merge-blocks const-lexical-values)
                          global-ids lexical-ids
                          (make-blocks '() (map merge-blocks body))
                          loc)]
    [_ (error "merge-blocks: not matched" node)]))

(define (make-blocks ids body)
  (let ([blocks (body->blocks body)])
    (if (and (null? (cdr blocks))
             (not (ormap contains-procedure-call? (car blocks))))
        (car blocks) ;; don't create blocks
        (list (s:blocks ids (map (lambda (b) (s:block '() b))
                                 blocks))))))


(define (body->blocks body)
  (let loop ([body (cdr body)]
             [acc (list (car body))]
             [blocks '()])
    (cond [(null? body)
           (reverse (cons (reverse acc) blocks))]
          [(contains-procedure-call? (car body))
           (loop (cdr body) (list (car body)) (cons (reverse acc) blocks))]
          [else
           (loop (cdr body) (cons (car body) acc) blocks)])))

(define (contains-procedure-call? node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)
         (s:primapp) (s:lambda) (s:dispatch-lambda)
         (s:loop))
     #f]
    [(s:app)
     #t]
    [(s:tail expr)
     (contains-procedure-call? expr)]
    [(s:if-true _ then)
     (contains-procedure-call? then)]
    [(s:if-false _ then)
     (contains-procedure-call? then)]
    [(s:lexical-init _ value)
     (contains-procedure-call? value)]
    [(s:global-init _ value)
     (contains-procedure-call? value)]
    [_ (error "merge-blocks contains-procedure-call?: not matched" node)]))

;; (define (body->blocks body)
;;   (let ([first-node-blocks (node->blocks (car body))])
;;     (let loop ([body (cdr body)]
;;                [acc (reverse (car first-node-blocks))]
;;                [blocks (reverse (cdr first-node-blocks))])
;;       (cond [(null? body)
;;              (reverse (cons (reverse acc) blocks))]
;;             [(contains-procedure-call? (car body))
;;              (match (node->blocks (car body))
;;                [(list block)
;;                 (loop (cdr body) block (cons (reverse acc) blocks))]
;;                [(list first middle ... last)
;;                 (loop (cdr body) last
;;                       (append (reverse middle)
;;                               (if (ormap contains-procedure-call? first)
;;                                   (cons first (cons (reverse acc) blocks))
;;                                   (cons (append (reverse acc) first) blocks))))])]
;;             [else
;;              (loop (cdr body) (cons (car body) acc) blocks)]))))

;; (define (node->blocks node)
;;   (match node
;;     [(s:if test then else loc)
;;      (let ([then-blocks (if (begin-stx? then)
;;                             (body->blocks (begin-stx-body then))
;;                             (body->blocks (list then)))]
;;            [else-blocks (if (begin-stx? else)
;;                             (body->blocks (begin-stx-body else))
;;                             (body->blocks (list else)))])
;;        (let-values ([(then-blocks else-blocks)
;;                      (fill-to-same-size then-blocks else-blocks)])
;;          (map list
;;               (append-map (lambda (then else)
;;                             (if (or (contains-wcmf? (car then))
;;                                     (contains-wcmf? (car else)))
;;                                 (list (make-if-stx loc test
;;                                                    (if (null? (cdr then))
;;                                                        (car then)
;;                                                        (s:begin then))
;;                                                    (s:literal #f))
;;                                       (make-if-stx loc test
;;                                                    (s:literal #f)
;;                                                    (if (null? (cdr else))
;;                                                        (car else)
;;                                                        (s:begin else))))
;;                                 (list (make-if-stx loc test
;;                                                    (if (null? (cdr then))
;;                                                        (car then)
;;                                                        (s:begin then))
;;                                                    (if (null? (cdr else))
;;                                                        (car else)
;;                                                        (s:begin else))))))
;;                           then-blocks else-blocks))))]
;;     [_ (list (list node))]))

;; (define (contains-wcmf? node)
;;   (match node
;;     [(or (s:literal) (s:lexical-ref) (s:global-ref)
;;          (s:primapp) (s:tail) (s:non-tail) (s:lambda) (s:dispatch-lambda))
;;      #f]
;;     [(s:begin body)
;;      (contains-wcmf? (car body))]
;;     [(s:if _ then else)
;;      (or (contains-wcmf? then)
;;          (contains-wcmf? else))]
;;     [(s:lexical-init _ value)
;;      (contains-wcmf? value)]
;;     [(s:wcmf _ expr _ _)
;;      #t]
;;     [_ (error "merge-blocks remove wcmf: not matched" node)]))

;; (define (fill-to-same-size l1 l2)
;;   (let ([len1 (length l1)] [len2 (length l2)])
;;     (cond [(= len1 len2) (values l1 l2)]
;;           [(< len1 len2) (let-values ([(l2 l1) (fill-to-same-size l2 l1)])
;;                            (values l1 l2))]
;;           [else          (values l1
;;                                  (append (make-list (- len1 len2) (list (s:literal #f)))
;;                                          l2))])))

(provide/contract
 [transform (stx? . -> . stx?)])
