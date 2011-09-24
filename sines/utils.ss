#lang scheme

(require syntax/stx)

(define (gen-temp)
  (with-syntax ([(t) (generate-temporaries '(1))])
    #'t))

(define (syntax-violation x y . rest)
  (error x "~A ~A" y rest))

(define (assertion-violation/conditions . rest)
  (error rest))

(define (syntax-violation/conditions . rest)
  (error rest))

(define (make-argument-name-condition)
  (error "not implemented"))

(define (make-keyword-condition)
  (error "not implemented"))

(define (make-predicate-expression-condition)
  (error "not implemented"))

(define (assertion-violation x y . rest)
  (error x "~A ~A" y rest))

(define (make-assertion-violation . args)
  (error "not implemented"))

(define (make-who-condition . args)
  (error "not implemented"))

(define (make-message-condition . args)
  (error "not implemented"))

(define (make-irritants-condition . args)
  (error "not implemented"))

(define (assert x)
  (unless x
    (error "assertion failed")))

(define pairwise?
  ;; Make a predicate which tests if all its arguments are pairwise true
  ;; for a given binary predicate.  0 and 1 arguments are always considered
  ;; true; e.g.: ((pairwise? <)) => #T and ((pairwise? =) 42) => #T.
  ;; The optional 2nd argument is an arbitrary procedure that takes 1
  ;; argument, and it is applied to each element once and must return a value
  ;; to use with the binary predicate, or raise an exception; this procedure
  ;; is useful for efficiently type-checking elements and/or transforming them.
  (case-lambda
    [(binary-pred)
     (pairwise? binary-pred #F)]
    [(binary-pred proc)
     (let ((next (if proc
                     (lambda (l) (proc (car l)))
                     car)))
       (lambda args
         (or (null? args)
             (let ((x (next args)))
               (let loop ((x x) (r (cdr args)))
                 (or (null? r)
                     (let ((y (next r)))
                       (and (binary-pred x y)
                            (loop y (cdr r))))))))))]))

(define name=?
  (pairwise? string=?
             (lambda (x)
               (cond [(identifier? x) (symbol->string (syntax->datum x))]
                     [(symbol? x) (symbol->string x)]
                     [(string? x) x]
                     [else (assertion-violation 'name=?
                                                "not an identifier, symbol, or string" x)]))))

(define (identifier?/name=? id name)
  (and (identifier? id)
       (name=? id name)))

  (define (make-arg-check-failed who)
    (lambda (pred-form arg-name arg-value)
      (assertion-violation/conditions who "argument check failed" (list arg-value)
        (make-argument-name-condition arg-name)
        (make-predicate-expression-condition pred-form))))

  (define (exact-non-negative-integer? x)
    (and (integer? x) (exact? x) (not (negative? x))))

  (define (_enumerate len)
    (define (_iota n l)
      (if (= n -1)
          l
          (_iota (- n 1) (cons n l))))
    (_iota (- len 1) '()))

(define (enumerate x)
  (write x) (newline)
  (_enumerate (cond [(list? x) (length x)]
                    [(vector? x) (vector-length x)]
                    [(string? x) (string-length x)]
                    [(bytes? x) (bytes-length x)])))

(define (formals-ok?/raise frmls-stx orig-stx)
  (syntax-case frmls-stx ()
    [(arg* ... . rest)
     (and (or (null? (syntax->datum #'rest))
              (identifier? #'rest)
              (syntax-violation #F "not an identifier" orig-stx #'rest))
          (andmap (lambda (id)
                    (or (identifier? id)
                        (syntax-violation #F "not an identifier" orig-stx id)))
                  (stx->list #'(arg* ...)))
          (unique-ids?/raise
           (append
            (stx->list #'(arg* ...))
            (if (identifier? #'rest) (list #'rest) '()))
           orig-stx))]))

(define (duplicate-id ids)
  (unless (and (list? ids) (andmap identifier? ids))
    (assertion-violation 'duplicate-id "not a list of identifiers" ids))
  (let recur ((ls ids))
    (and (pair? ls)
         (let ((id (car ls)) (rest (cdr ls)))
           (if (memf (lambda (x) (bound-identifier=? x id)) rest)
               id
               (recur (cdr ls)))))))

(define (unique-ids? ls)
  (not (duplicate-id ls)))

(define unique-ids?/raise
  (case-lambda
    [(ids orig-stx msg)
     (let ((dup (duplicate-id ids)))
       (if dup
           (syntax-violation #F msg orig-stx dup)
           #T))]
    [(ids orig-stx)
     (unique-ids?/raise ids orig-stx "duplicate identifier")]))

(define condition
  (lambda args
    (error "Not YET")))

(define with-exception-handler call-with-exception-handler)

(define reraise error)

(provide gen-temp
         syntax-violation
         assertion-violation
         identifier?/name=?
         enumerate
         formals-ok?/raise
         unique-ids?/raise
         syntax-violation/conditions
         make-keyword-condition
         assert
         condition
         make-assertion-violation
         make-who-condition
         make-message-condition
         make-keyword-condition
         make-predicate-expression-condition
         make-irritants-condition
         with-exception-handler
         provide reraise)