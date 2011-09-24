#lang s-exp "../base-lang.rkt"

(require
 (for-syntax scheme/base)
 (for-syntax scheme/list)
 (for-syntax scheme/match)
 (for-syntax syntax/stx)
 (for-syntax "../compiler/store.rkt"))

(define-syntax (define-getter stx)
  (syntax-case stx ()
    [(_ (name object) property-name)
     (and (identifier? #'name) (identifier? #'object))
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       #'(define (name object)
           (es-property object property-name)))]
    [(_ (name) property-name object)
     (identifier? #'name)
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       #'(define (name)
           (es-property object property-name)))]))

(define-syntax (define-setter stx)
  (syntax-case stx ()
    [(_ (name object value) property-name)
     (and (identifier? #'name) (identifier? #'object) (identifier? #'value))
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       #'(define (name object value)
           (es-property-set! object property-name value)))]
    [(_ (name object) property-name value)
     (and (identifier? #'name) (identifier? #'object))
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       #'(define (name object)
           (es-property-set! object property-name value)))]
    [(_ (name) property-name object value)
     (identifier? #'name)
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       #'(define (name)
           (es-property-set! object property-name value)))]))

(define-syntax (define-getter+setter stx)
  (define (setter-name name)
    (datum->syntax name (string->symbol
                         (string-append
                          (symbol->string
                           (syntax->datum name))
                          "-set!"))))
  (syntax-case stx ()
    [(_ (name object) property-name)
     (and (identifier? #'name) (identifier? #'object))
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       (with-syntax ([name-set! (setter-name #'name)]
                     [(value property-name2) (generate-temporaries #'(name name))])
         #'(begin
             (define property-name2 property-name)
             (define-getter (name object) property-name2)
             (define-setter (name-set! object value) property-name2))))]
    [(_ (name object) property-name value)
     (and (identifier? #'name) (identifier? #'object))
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       (with-syntax ([name-set! (setter-name #'name)]
                     [(property-name2) (generate-temporaries #'(name))])
         #'(begin
             (define property-name2 property-name)
             (define-getter (name object) property-name2)
             (define-setter (name-set! object) property-name2 value))))]
    [(_ (name) property-name object)
     (identifier? #'name)
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       (with-syntax ([name-set! (setter-name #'name)]
                     [(value property-name2) (generate-temporaries #'(name name))])
         #'(begin
             (define property-name2 property-name)
             (define-getter (name) property-name2 object)
             (define-setter (name-set! value) property-name2 object))))]
    [(_ (name) property-name object value)
     (identifier? #'name)
     (with-syntax ([property-name (if (not (syntax-e #'property-name))
                                      #'(es-property-name name)
                                      #'property-name)])
       (with-syntax ([name-set! (setter-name #'name)]
                     [(property-name2) (generate-temporaries #'(name))])
         #'(begin
             (define property-name2 property-name)
             (define-getter (name) property-name2 object)
             (define-setter (name-set!) property-name2 object value))))]))

(begin-for-syntax
  (define (check-params params)
    (cond [(findf (lambda (p)
                    (not (or (identifier? p)
                             (and (stx-pair? p)
                                  (identifier? (stx-car p))
                                  (stx-pair? (stx-cdr p))
                                  (stx-null? (stx-cdr (stx-cdr p)))))))
                  params)
           => (lambda (p) (raise-syntax-error #f "must be an identifier" p))]))
  (define (params-names params)
    (map (lambda (p)
           (if (identifier? p)
               p
               (stx-car p)))
         params)))

(define-syntax (define-function-invoker stx)
  (define (params-without params arg)
    (remove arg params (lambda (a b) (and (identifier? a) (free-identifier=? a b)))))
  (define (actual-args params args)
    (if (null? args)
        params
        (cons (car args)
              (actual-args (params-without params (car args))
                           (cdr args)))))
  (define (optional? params)
    (define (optionals-followed-by-identifiers? params)
      (or (null? params)
          (syntax-case (car params) ()
            [(x) (and (identifier? #'x) (optionals-followed-by-identifiers? (cdr params)))]
            [_   (andmap identifier? params)])))
    (and (not (null? params))
         (let ([params (reverse params)])
           (syntax-case (car params) ()
             [(x) (and (identifier? #'x) (optionals-followed-by-identifiers? (cdr params)))]
             [_   #f]))))
  (syntax-case stx ()
    [(_ (name param ...) function this-object arg ...)
     (and (identifier? #'name) (optional? (syntax->list #'(param ...))))
     (let-values ([(required optional) (partition identifier? (syntax->list #'(param ...)))])
       (let ([optional (map (lambda (p) (car (syntax->list p))) optional)])
         (let ([paramss (reverse (foldl (lambda (a more)
                                          (cons (append (car more) (list a)) more))
                                        (list required)
                                        optional))])
           (let ([actual-argss (map (lambda (params) (actual-args (params-without (params-names params) #'this-object) (syntax->list #'(arg ...)))) paramss)])
             (with-syntax ([((param ...) ...) paramss]
                           [((arg ...) ...) actual-argss])
               (syntax-case #'function (es-property es-global)
                 [(es-property object property-name)
                  (if (equal? (syntax->datum #'object) (syntax->datum #'this-object))
                      #'(define name
                          (case-lambda
                            [(param ...)
                             (es-call-property object property-name arg ...)] ...))
                      #'(define name
                          (case-lambda
                            [(param ...)
                             (es-call-property function "call" this-object arg ...)] ...)))]
                 [(es-global name)
                  (free-identifier=? #'this-object #'null)
                  #'(define name
                      (case-lambda
                        [(param ...)
                         (es-call function arg ...)] ...))]))))))]
    [(_ (name param ...) function this-object arg ...)
     (identifier? #'name)
     (let ([params (syntax->list #'(param ...))])
       (check-params params)
       (let ([all-args (actual-args (params-without (params-names params) #'this-object) (syntax->list #'(arg ...)))])
         (with-syntax ([(arg ...) all-args])
           (syntax-case #'function (es-property)
             [(es-property object property-name)
              (equal? (syntax->datum #'object) (syntax->datum #'this-object))
              #'(define (name param ...)
                  (es-call-property object property-name arg ...))]
             [_
              #'(define (name param ...)
                  (es-call-property function "call" this-object arg ...))]))))]
    [(_ (name param ... . rest-param) function arg ...)
     (identifier? #'name)
     (let ([params (syntax->list #'(param ...))]
           (args (syntax->list #'(arg ...))))
       (check-params params)
       (unless (identifier? #'rest-param)
         (raise-syntax-error #f "must be an identifier" #'rest-param))
       (cond [(findf (lambda (p) (and (identifier? p)
                                      (free-identifier=? p #'rest-param)))
                     args)
              => (lambda (p) (raise-syntax-error #f "must not be used" p))])
       (let ([all-args (actual-args (params-names params) args)])
         (with-syntax ([this-object (car all-args)]
                       [(arg ...) (cdr all-args)])
           #'(define (name param ... . rest-param)
               (es-apply-to/list function this-object arg ... rest-param)))))]))

(define-syntax (define-invoker stx)
  (syntax-case stx ()
    [(_ (name object . params) property-name)
     #'(define-invoker (name object . params) property-name object)]
    [(_ (name . params) #f object arg ...)
     (identifier? #'name)
     #'(define-function-invoker (name . params) (es-property object (es-property-name name)) object arg ...)]
    [(_ (name . params) property-name object arg ...)
     (identifier? #'name)
     #'(define-function-invoker (name . params) (es-property object (es-property-name! name property-name)) object arg ...)]))

(define-syntax (define-primitive-invoker stx)
  (syntax-case stx ()
    [(_ (name . params) primitive arg ...)
     (identifier? #'name)
     #'(define-function-invoker (name . params) primitive null arg ...)]))

(define-syntax (define-constructor-invoker stx)
  (syntax-case stx ()
    [(_ (name param ...) primitive)
     (identifier? #'name)
     (let ([params (syntax->list #'(param ...))])
       (check-params params)
       (with-syntax ([(param-name ...) (params-names params)])
         #'(define (name param ...)
             (es-new primitive param-name ...))))]))

(define-syntax (define-method-on-prototype stx)
  (syntax-case stx ()
    [(_ (name (this function-object) param ...) . body)
     (and (identifier? #'name)
          (identifier? #'this)
          (identifier? #'function-object))
     (let ([params (syntax->list #'(param ...))])
       (check-params params)
       #'(es-property-set! (es-property function-object "prototype")
                           (es-property-name name)
                           (wrap-for-callback
                            (lambda (this param ...) . body)
                            #t)))]))

(define-syntax (define-method-on-object stx)
  (syntax-case stx ()
    [(_ (name (this function-object) param ...) . body)
     (and (identifier? #'name)
          (identifier? #'this))
     (let ([params (syntax->list #'(param ...))])
       (check-params params)
       #'(es-property-set! function-object
                           (es-property-name name)
                           (wrap-for-callback
                            (lambda (this param ...) . body)
                            #t)))]))

(define-syntax (define-identifier-syntax stx)
  (syntax-case stx ()
    [(_ name template)
     #'(...
        (define-syntax name
          (make-set!-transformer
           (lambda (stx)
             (syntax-case stx (set!)
               [(set! . _) (raise-syntax-error
                            #f
                            "cannot assign to identifier macro"
                            stx)]
               [(_ arg ...) #'(template arg ...)]
               [_ #'template])))))]))


(provide define-getter define-setter define-getter+setter
         define-invoker
         define-primitive-invoker
         define-constructor-invoker
         define-method-on-prototype
         define-method-on-object
         define-identifier-syntax)

(define-invoker (string->json string)
  "parse" (es-global "JSON"))

(define-invoker (json->string object)
  "stringify" (es-global "JSON"))

(define (es-null->false form)
  (if (null? form)
      #f
      form))

(provide string->json
         json->string
         es-null->false)
