#lang scheme/base

(require scheme/base
         scheme/dict
         scheme/list
         syntax/id-table
         syntax/stx
         (for-template scheme/base)
         (for-template "syntax.rkt"))

(define (syntax->loc stx)
  (quasisyntax/loc
   stx
   (make-loc #,(syntax-source stx)
             #,(syntax-position stx)
             #,(syntax-line stx)
             #,(syntax-column stx)
             #,(syntax-span stx))))

(define (module-syntax->free-id-mappings stx)
  (syntax-case stx (#%plain-module-begin)
    [(#%plain-module-begin . body)
     (apply append (filter-map syntax->free-id-mappings (syntax->list #'body)))]))

(define (syntax->free-id-mappings stx)
  (syntax-case stx (begin define-values)
    [(begin . body)
     (apply append (filter-map syntax->free-id-mappings (syntax->list #'body)))]
    [(define-values (id ...) value)
     (map (lambda (id) (cons id (make-free-id-mapping! id)))
          (stx->list #'(id ...)))]
    [_ #f]))

(define (module-syntax->stx stx)
  (syntax-case stx (#%plain-module-begin)
    [(#%plain-module-begin . body)
     #`(make-module-stx #,(syntax->loc stx)
                        (list #,@(filter-map syntax->stx (syntax->list #'body))))]))

(define (maybe-begin body)
  (if (null? (cdr body))
      (car body)
      #`(make-begin-stx #f (list #,@body))))

(define (syntax->stx stx)
  (syntax-case stx (#%expression
                    #%plain-app
                    #%plain-lambda
                    #%require
                    #%provide
                    begin
                    begin0
                    case-lambda
                    define-syntaxes
                    define-values
                    define-values-for-syntax
                    if
                    let-values
                    letrec-syntaxes+values
                    letrec-values
                    quote
                    quote-syntax
                    set!
                    with-continuation-mark)
    [(#%expression a)
     (syntax->stx #'a)]
    [(#%plain-app op . args)
     #`(make-app-stx #,(syntax->loc stx)
                     #,(syntax->stx #'op)
                     (list #,@(map syntax->stx (syntax->list #'args))))]
    [(#%plain-lambda params . body)
     (syntax-case #'params ()
       [(param ...)
        #`(make-lambda-stx #,(syntax->loc stx)
                           (list #,@(map (lambda (id)
                                           (syntax->id id #t))
                                         (syntax->list #'(param ...))))
                           #f
                           #,(maybe-begin
                              (map syntax->stx (syntax->list #'body))))]
       [(param ... . rest-param)
        #`(make-lambda-stx #,(syntax->loc stx)
                           (list #,@(map (lambda (id)
                                           (syntax->id id #t))
                                         (syntax->list #'(param ...))))
                           #,(syntax->id #'rest-param #t)
                           #,(maybe-begin
                              (map syntax->stx (syntax->list #'body))))])]
    [(#%provide . _)
     #f]
    [(#%require . _)
     #f]
    [(begin . body)
     #`(make-begin-stx #,(syntax->loc stx)
                       (list #,@(map syntax->stx (syntax->list #'body))))]
    [(case-lambda . clauses)
     (begin
       (define (handle-clause clause)
         (syntax-case clause ()
           [((param ...) . body)
            #`(make-case-lambda-clause-stx #,(syntax->loc clause)
                                           (list #,@(map (lambda (id)
                                                      (syntax->id id #t))
                                                    (syntax->list #'(param ...))))
                                           #f
                                           #,(maybe-begin
                                              (map syntax->stx (syntax->list #'body))))]
           [((param ... . rest-param) . body)
            #`(make-case-lambda-clause-stx #,(syntax->loc clause)
                                           (list #,@(map (lambda (id)
                                                           (syntax->id id #t))
                                                         (syntax->list #'(param ...))))
                                           #,(syntax->id #'rest-param #t)
                                           #,(maybe-begin
                                              (map syntax->stx (syntax->list #'body))))]))
       #`(make-case-lambda-stx #,(syntax->loc stx)
                               (list #,@(map handle-clause (syntax->list #'clauses)))))]
    [(define-syntaxes . _)
     #f]
    [(define-values (id ...) value)
     #`(make-define-values-stx #,(syntax->loc stx)
                               (list #,@(map (lambda (id) (syntax->module-id id (identifier-binding id))) (stx->list #'(id ...))))
                               #,(syntax->stx #'value))]
    [(define-values-for-syntax . _)
     #f]
    [(if test then else)
     #`(make-if-stx #,(syntax->loc stx)
                    #,(syntax->stx #'test)
                    #,(syntax->stx #'then)
                    #,(syntax->stx #'else))]
    [(let-values ([ids value] ...) . body)
     #`(make-let-values-stx #,(syntax->loc stx)
                            (list #,@(map (lambda (ids)
                                            #`(list #,@(map (lambda (id) (syntax->id id #t))
                                                            (syntax->list ids))))
                                          (syntax->list #'(ids ...))))
                            (list #,@(map syntax->stx (syntax->list #'(value ...))))
                            #,(maybe-begin
                               (map syntax->stx (syntax->list #'body))))]
    [(letrec-syntaxes+values syntaxes values . body)
     (syntax->stx (syntax/loc stx (letrec-values values . body)))]
    [(letrec-values ([ids value] ...) . body)
     #`(make-letrec-values-stx #,(syntax->loc stx)
                               (list #,@(map (lambda (ids)
                                               #`(list #,@(map (lambda (id) (syntax->id id #t))
                                                               (syntax->list ids))))
                                             (syntax->list #'(ids ...))))
                               (list #,@(map syntax->stx (syntax->list #'(value ...))))
                               #,(maybe-begin
                                  (map syntax->stx (syntax->list #'body))))]
    [(quote a)
     #`(make-literal-stx #,(syntax->loc #'a)
                         '#,(syntax->datum #'a))]
    [(set! var value)
     #`(make-set!-stx #,(syntax->loc stx)
                      #,(syntax->stx #'var)
                      #,(syntax->stx #'value))]
    [(with-continuation-mark key value result)
     #`(make-wcm-stx #,(syntax->loc stx)
                     #,(syntax->stx #'key)
                     #,(syntax->stx #'value)
                     #,(syntax->stx #'result))]
    [id
     (identifier? #'id)
     (let ([binding (identifier-binding #'id)])
       (if (eq? 'lexical binding)
           #`(make-lexical-ref-stx #,(syntax->loc #'id) #,(syntax->id #'id #f))
           #`(make-global-ref-stx #,(syntax->loc #'id)
                                  #,(syntax->module-id #'id binding))))]))

(define free-id-table (make-free-id-table))

(define (make-free-id-mapping! stx)
  (let ([unique-string (string-append (number->string (random 1000000))
                                      "."
                                      (number->string (current-inexact-milliseconds)))])
    (add-free-id-mapping! stx unique-string)
    unique-string))

(define (free-id-lookup stx)
  (dict-ref free-id-table stx))

(define (add-free-id-mapping! stx string)
  (dict-set! free-id-table stx string))

(provide add-free-id-mapping!
         make-free-id-mapping!
         free-id-lookup)

(define (syntax->module-id stx binding)
  (with-syntax ([orig-name (second binding)]
                [r (dict-ref free-id-table stx)])
    #'(make-module-id 'orig-name r)))

(define syntax->id
  (let ([c 0]
        [dict (make-bound-id-table)])
    (lambda (stx binding?)
      (if binding?
          (let ([id #`(make-id '#,(syntax->datum stx) #,c)])
            (set! c (add1 c))
            (dict-set! dict stx id)
            id)
          (dict-ref dict stx)))))

(provide module-syntax->free-id-mappings
         module-syntax->stx
         syntax->stx)
