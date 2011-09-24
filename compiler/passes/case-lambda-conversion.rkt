#lang scheme

(require
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt"))

(define (transform node)
  (map-node generic-transform node))

(define (generic-transform node recur proceed)
  (match node
    [(s:case-lambda clauses loc)
     (let ([new-clauses (map (lambda (clause)
                               (match clause
                                 [(s:case-lambda-clause args rest-arg body loc)
                                  (s:lambda args rest-arg (recur body) loc)]))
                             clauses)])
       (if (= 1 (length clauses))
           (car new-clauses)
           (s:dispatch-lambda new-clauses loc)))]
    ;; [(s:let-values ids values body loc)
    ;;  (match ids
    ;;    [(list (list) more ...)
    ;;     (error 'case-lambda-conversion "unsuported let-values use")]
    ;;    [(list (list id1 id2 more-ids ...))
    ;;     (s:app (s:global-ref (sines-variable-id l:call-with-values) loc)
    ;;            (list (s:lambda '() #f (recur (car values)) loc)
    ;;                  (s:lambda (list* id1 id2 more-ids) #f (recur body) loc))
    ;;            loc)]
    ;;    [(list (list _) ...)
    ;;     (s:let-values ids (map recur values) (recur body) loc)]
    ;;    [_ (error "case-lambda-conversion: unsuported let-values use 2")])]
    [_ (proceed node)]))

(provide/contract
 [transform (stx? . -> . stx?)])

