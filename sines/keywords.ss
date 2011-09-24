#lang scheme

(require
 (for-syntax syntax/stx)
 "utils.ss"
 (for-syntax "utils.ss")
 "keywords2.ss"
 (for-syntax "keywords2.ss"))

(begin-for-syntax
 (define (parse-kw-formals full-stx kw-formals)
   (let parse ([kwf kw-formals] [pos-ids '()])
     (syntax-case kwf ()
       [((kw-id . opts) ... . additional-id)
        (let ([pos-ids (reverse pos-ids)])
          (and (formals-ok?/raise (append pos-ids (stx->list #'(kw-id ... . additional-id)))
                                  full-stx)
               (list* pos-ids
                      #'((kw-id . opts) ...)
                      (if (identifier? #'additional-id)
                          (list #'additional-id)
                          '()))))]
       [(pos . r)
        (parse #'r (cons #'pos pos-ids))])))

 (define (args-parser stx)
   (define (gen-stx who kw=? missing-value missing-keyword predicate-false
                    kw-values process-input-list car-id cdr-id additional)
     (lambda (kw-spec index)
       (with-syntax (((kw-id . _) kw-spec)
                     (v (gen-temp)))
         (define (gen-clause/val)
           #`((#,kw=? #,car-id 'kw-id)
              (if (pair? #,cdr-id)
                  (vector-set! #,kw-values #,index (car #,cdr-id))
                  (#,missing-value '#,who 'kw-id))
              (#,process-input-list (cdr #,cdr-id) #,additional)))
         (define (gen-test/val true false)
           #`(let ((v (vector-ref #,kw-values #,index)))
               (if (not-given? v) #,false #,true)))
         (define (gen-pred/val pred)
           #`(if (#,pred v)
                 v
                 (#,predicate-false '#,who 'kw-id '#,pred v)))
         (define (gen-missing)
           #`(#,missing-keyword '#,who 'kw-id))
         (define (gen-clause/bool)
           #`((#,kw=? #,car-id 'kw-id)
              (vector-set! #,kw-values #,index #T)
              (#,process-input-list #,cdr-id #,additional)))
         (define (gen-test/bool)
           #`(not (not-given? (vector-ref #,kw-values #,index))))
         (let-values ([(default predicate boolean) (process-options stx kw-spec)])
           (cond [(and default predicate)
                  (list (gen-clause/val)
                        (gen-test/val (gen-pred/val predicate) default))]
                 [default
                   (list (gen-clause/val)
                         (gen-test/val #'v default))]
                 [predicate
                  (list (gen-clause/val)
                        (gen-test/val (gen-pred/val predicate) (gen-missing)))]
                 [boolean
                  (list (gen-clause/bool)
                        (gen-test/bool))]
                 [else
                  (list (gen-clause/val)
                        (gen-test/val #'v (gen-missing)))])))))
   (syntax-case stx ()
     [(_ who kw=? missing-value missing-keyword predicate-false
         (kw-id options ...) ...)
      (andmap identifier?
              (stx->list #'(kw=? missing-value missing-keyword predicate-false kw-id ...)))
      (with-syntax ([num (length (stx->list #'(kw-id ...)))]
                    [(kw-values process-input-list car-id cdr-id additional)
                     (generate-temporaries '(1 2 3 4 5))])
        (with-syntax ([((cond-clause value-expr) ...)
                       (map (gen-stx #'who #'kw=?
                                     #'missing-value #'missing-keyword #'predicate-false
                                     #'kw-values #'process-input-list
                                     #'car-id #'cdr-id #'additional)
                            (stx->list #'((kw-id options ...) ...))
                            (enumerate (stx->list #'(kw-id ...))))])
          #'(lambda (input-list)
              (let ([kw-values (make-vector num not-given)])
                (let process-input-list ([l (stx->list input-list)]
                                         [additional '()])
                  (cond [(pair? l)
                         (let ([car-id (car l)] [cdr-id (cdr l)])
                           (cond cond-clause ...
                                 (else (process-input-list cdr-id
                                                           (cons car-id additional)))))]
                        [(null? l)
                         (define kw-id value-expr) ...
                         (values kw-id ... (reverse additional))]
                        [else
                         (assertion-violation 'who "not a proper list" input-list)]))))))]))

 (define (predicate-false--dummy . _)
   (assert #F))

 (define (kw-stx=? x y)
   (syntax-case x (quote)
     [(quote id) (identifier? #'id)
      (eq? (syntax->datum #'id) y)]
     [_ #f])))

(define-syntax (define/kw stx)
  ;; Optimized to process keywords at expand time.
  (define (gen-stx who)
    (lambda (kw-spec kw-value)
      (with-syntax ([(kw-id . _) kw-spec])
        (let-values ([(default predicate boolean) (process-options stx kw-spec)])
          (cond [(and default predicate)
                 (list #`(if (not-given? #,kw-value)
                             #,default
                             (if (#,predicate #,kw-value)
                                 #,kw-value
                                 (predicate-false--default '#,who 'kw-id
                                                           '#,predicate #,kw-value)))
                       #'(:default #'not-given))]
                [default
                  (list #`(if (not-given? #,kw-value)
                              #,default
                              #,kw-value)
                        #'(:default #'not-given))]
                [predicate
                 (list #`(if (#,predicate #,kw-value)
                             #,kw-value
                             (predicate-false--default '#,who 'kw-id
                                                       '#,predicate #,kw-value))
                       '())]
                [boolean
                 (list kw-value #'(:boolean))]
                [else
                 (list kw-value '())])))))
  (syntax-case stx ()
    [(_ (name . kw-formals) . body)
     (with-syntax ([((pos-id ...) ((kw-id . kw-opts) ...) additional-id ...)
                    (parse-kw-formals stx #'kw-formals)])
       (with-syntax ([(kw-value ...) (generate-temporaries #'(kw-id ...))])
         (with-syntax ([((value-expr kw-stx-opts) ...)
                        (map (gen-stx #'name)
                             (stx->list #'((kw-id . kw-opts) ...))
                             (stx->list #'(kw-value ...)))])
           #'(begin
               (define (proc pos-id ... kw-id ... additional-id ...)
                 . body)
               (define (proc/ve pos-id ... kw-value ... additional-id ...)
                 (define kw-id value-expr) ...
                 (proc pos-id ... kw-id ... additional-id ...))
               (define-syntax (name stx)
                 (define parser
                   (let ([mv (lambda (who kw) (syntax-violation/conditions who "keyword missing value" stx #F (make-keyword-condition kw)))]
                         [mk (lambda (who kw) (syntax-violation/conditions who "missing required keyword" stx #F (make-keyword-condition kw)))])
                     (args-parser 'name mv mk predicate-false--dummy (kw-id . kw-stx-opts) ...)))
                 (syntax-case stx ()
                   [(_ pos-id ... kw-expr (... ...))
                    ;; NOTE: Only (quote <identifier>) forms are recognized as
                    ;; keywords, unlike run-time processing which recognizes
                    ;; symbol values; and when multiple occurances of a
                    ;; keyword are present, only the last one's expression is
                    ;; evaluated; and additional expressions are evaluated
                    ;; only when the define/kw formals specified taking
                    ;; additionals.
                    ;; TODO?: Always evaluate all expressions?
                    ;; But it would still differ in that only 'kw are recognized.
                    (with-syntax ([(kw-expr/ordered (... ...) (additional-expr  (... ...)))
                                   (call-with-values (lambda () (parser #'(kw-expr (... ...)))) (lambda v v))])
                      (with-syntax ([(additional-expr (... ...))
                                     (cond [(positive? (length '(additional-id ...)))
                                            (list #'(list additional-expr (... ...)))]
                                           [(positive? (length (stx->list #'(additional-expr (... ...)))))
                                            (syntax-violation #f "unexpected additional expressions" stx #'(additional-expr (... ...)))]
                                           [else '()])])
                        #'(proc/ve pos-id ... kw-expr/ordered (... ...) additional-expr (... ...))))]
                   [id
                    (identifier? #'id)
                    #'first-class]))))))]))

(provide define/kw)
