#lang s-exp "../base-lang.rkt"

(require
 (for-syntax scheme/base)
 (for-syntax scheme/list)
 (for-syntax scheme/match)
 (for-syntax syntax/stx)
 (for-syntax "../compiler/store.rkt")
 "../racket/match/match.ss"
 "bridge.ss"
 "to-string.ss")

(define-syntax (define-prototype stx)
  (define (slot-names slots-stx)
    (map (lambda (slot-spec)
           (syntax-case slot-spec ()
             [(id #:mutable #:string-name _)
              (identifier? #'id)
              #'id]
             [(id #:string-name _ #:mutable)
              (identifier? #'id)
              #'id]
             [(id #:mutable)
              (identifier? #'id)
              #'id]
             [(id #:string-name _)
              (identifier? #'id)
              #'id]
             [(id)
              (identifier? #'id)
              #'id]
             [id
              (identifier? #'id)
              #'id]))
         (syntax->list slots-stx)))
  (define (accessor-names name-stx slots-stx slot-names-stx)
    (define (accessor-name name-stx slot-name-stx)
      (datum->syntax name-stx
                     (string->symbol (string-append (symbol->string (syntax->datum name-stx))
                                                    "-"
                                                    (symbol->string (syntax->datum slot-name-stx))))))
    (map (lambda (stx)
           (accessor-name name-stx stx))
         (syntax->list slot-names-stx)))
  (define (maker-name name-stx)
    (datum->syntax name-stx
                   (string->symbol
                    (string-append "make-"
                                   (symbol->string
                                    (syntax->datum name-stx))))))
  (define (predicate-name name-stx)
    (datum->syntax name-stx
                   (string->symbol
                    (string-append (symbol->string
                                    (syntax->datum name-stx))
                                   "?"))))
  (define (constructor-params slots-stx)
    (generate-temporaries slots-stx))
  (define (getter-names slots-stx accessor-names-stx)
    (filter-map (lambda (slot-spec name)
                  (syntax-case slot-spec ()
                    [(id #:mutable . _)   #f]
                    [(id #:string-name _ #:mutable) #f]
                    [(id #:string-name _) name]
                    [(id) (identifier? #'id) name]
                    [id (identifier? #'id) name]))
                (syntax->list slots-stx)
                (syntax->list accessor-names-stx)))
  (define (setter-names slots-stx accessor-names-stx)
    (filter-map (lambda (slot-spec name)
                  (syntax-case slot-spec ()
                    [(id #:mutable . _)   name]
                    [(id #:string-name _ #:mutable) name]
                    [(id #:string-name _) #f]
                    [(id) (identifier? #'id) #f]
                    [id (identifier? #'id) #f]))
                (syntax->list slots-stx)
                (syntax->list accessor-names-stx)))
  (define (getter-string-names slots-stx)
    (filter-map (lambda (slot-spec)
                  (syntax-case slot-spec ()
                    [(_ #:mutable #:string-name name) #f]
                    [(_ #:mutable) #f]
                    [(_ #:string-name name #:mutable) #f]
                    [(_ #:string-name name) #'name]
                    [(id) (identifier? #'id) #'#f]
                    [id (identifier? #'id) #'#f]))
                (syntax->list slots-stx)))
  (define (setter-string-names slots-stx)
    (filter-map (lambda (slot-spec)
                  (syntax-case slot-spec ()
                    [(_ #:mutable #:string-name name) #'name]
                    [(_ #:mutable) #'#f]
                    [(_ #:string-name name #:mutable) #'name]
                    [(_ #:string-name _) #f]
                    [(id) (identifier? #'id) #f]
                    [id (identifier? #'id) #f]))
                (syntax->list slots-stx)))
  (define (getter-and-setter-string-names slots-stx accessor-names-stx)
    (map (lambda (slot-spec accessor-name)
           (syntax-case slot-spec ()
             [(_ #:mutable #:string-name name) #'name]
             [(_ #:mutable) #`(es-property-name #,accessor-name)]
             [(_ #:string-name name #:mutable) #'name]
             [(_ #:string-name name) #'name]
             [(id) (identifier? #'id) #`(es-property-name #,accessor-name)]
             [id (identifier? #'id) #`(es-property-name #,accessor-name)]))
         (syntax->list slots-stx)
         (syntax->list accessor-names-stx)))
  (syntax-case stx ()
    [(_ name (slot ...))
     (identifier? #'name)
     (with-syntax ([(slot-name ...) (slot-names #'(slot ...))])
       (with-syntax ([(accessor-name ...) (accessor-names #'name #'(slot ...) #'(slot-name ...))])
         (with-syntax ([maker-name (maker-name #'name)]
                       [predicate-name (predicate-name #'name)]
                       [(param-name ...) (constructor-params #'(slot ...))]
                       [(getter ...) (getter-names #'(slot ...) #'(accessor-name ...))]
                       [(setter ...) (setter-names #'(slot ...) #'(accessor-name ...))]
                       [(property-name ...) (getter-and-setter-string-names #'(slot ...) #'(accessor-name ...))]
                       [(getter-name ...) (getter-string-names #'(slot ...))]
                       [(setter-name ...) (setter-string-names #'(slot ...))]
                       [to-string-string (string-append "#<" (symbol->string (syntax->datum #'name)))])
           #'(begin
               (define-syntax name
                 (make-prototype-fields #'constructor-name
                                        #'maker-name
                                        #'predicate-name
                                        (list #'slot-name ...)
                                        (list #'accessor-name ...)
                                        (list #'param-name ...)
                                        (list #'property-name ...)))
               (define constructor-name (empty-function-procedure))
               (define-getter (getter name) getter-name) ...
               (define-getter+setter (setter name) setter-name) ...
               (define (maker-name param-name ...)
                 (let ([x (es-new constructor-name)])
                   (es-property-set! x property-name param-name) ...
                   x))
               (define (predicate-name obj)
                 (es-instance? obj constructor-name))
               (define-method-on-prototype (es-to-string (obj constructor-name))
                 (string-append to-string-string
                                (string-append " " (write-to-string (accessor-name obj))) ...
                                ">"))))))]
    [(_ (name parent) (slot ...))
     (and (identifier? #'name) (identifier? #'parent))
     (let ([parent-fields (syntax-local-value #'parent)])
       (with-syntax ([(slot-name ...) (slot-names #'(slot ...))])
         (with-syntax ([(accessor-name ...) (accessor-names #'name #'(slot ...) #'(slot-name ...))]
                       [parent-proc (prototype-fields-constructor-name parent-fields)]
                       [(parent-accessor-name ...) (prototype-fields-getter-names parent-fields)]
                       [(parent-slot-name ...) (prototype-fields-slot-names parent-fields)]
                       [(parent-param-name ...) (prototype-fields-param-names parent-fields)]
                       [(parent-property-name ...) (prototype-fields-property-names parent-fields)])
           (with-syntax ([maker-name (maker-name #'name)]
                         [predicate-name (predicate-name #'name)]
                         [(param-name ...) (constructor-params #'(slot ...))]
                         [(getter ...) (getter-names #'(slot ...) #'(accessor-name ...))]
                         [(setter ...) (setter-names #'(slot ...) #'(accessor-name ...))]
                         [(property-name ...) (getter-and-setter-string-names #'(slot ...) #'(accessor-name ...))]
                         [(getter-name ...) (getter-string-names #'(slot ...))]
                         [(setter-name ...) (setter-string-names #'(slot ...))]
                         [to-string-string (string-append "#<" (symbol->string (syntax->datum #'name)))])
             #'(begin
                 (define-syntax name
                   (make-prototype-fields #'constructor-name
                                          #'maker-name
                                          #'predicate-name
                                          (list #'parent-slot-name ... #'slot-name ...)
                                          (list #'parent-accessor-name ... #'accessor-name ...)
                                          (list #'parent-param-name ... #'param-name ...)
                                          (list #'parent-property-name ... #'property-name ...)))
                 (define constructor-name (empty-function-procedure))
                 (es-property-set! constructor-name "prototype" (es-new parent-proc))
                 (es-property-set! (es-property constructor-name "prototype") "constructor" constructor-name)
                 (define-getter (getter name) getter-name) ...
                 (define-getter+setter (setter name) setter-name) ...
                 (define (maker-name parent-param-name ... param-name ...)
                   (let ([x (es-new constructor-name)])
                     (es-property-set! x parent-property-name parent-param-name) ...
                     (es-property-set! x property-name param-name) ...
                     x))
                 (define (predicate-name obj)
                   (es-instance? obj constructor-name))
                 (define-method-on-prototype (es-to-string (obj constructor-name))
                   (string-append to-string-string
                                  (string-append " " (write-to-string (parent-accessor-name obj))) ...
                                  (string-append " " (write-to-string (accessor-name obj))) ...
                                  ">")))))))]))

(define-invoker (es-to-string obj) "toString")

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

(define-syntax (define-method stx)
  (syntax-case stx ()
    [(_ (name [this function-object] . params) . body)
     (and (identifier? #'name)
          (identifier? #'this)
          (identifier? #'function-object))
     (with-syntax ([function-object (prototype-fields-constructor-name
                                     (syntax-local-value #'function-object))])
       #'(define-method-on-prototype (name [this function-object] . params) . body))]))

(define-syntax (define-prototype-generic stx)
  (syntax-case stx ()
    [(_ (name object param ...))
     (and (identifier? #'name)
          (identifier? #'object))
     (let ([params (syntax->list #'(param ...))])
       (check-params params)
       (with-syntax ([(param-name ...) (params-names params)])
         #'(define (name object param ...)
             ((es-property object (es-property-name name)) object param-name ...))))]
    [(_ (name object param ... . last-param))
     (and (identifier? #'name)
          (identifier? #'object)
          (identifier? #'last-param))
     (let ([params (syntax->list #'(param ...))])
       (check-params (syntax->list #'(param ...)))
       (with-syntax ([(param-name ...) (params-names params)])
         #'(define (name object param ... . last-param)
             (apply (es-property object (es-property-name name)) object param-name ... last-param))))]))

(define-syntax (define-prototype-method stx)
  (syntax-case stx ()
    [(_ (name [this function-object] . params) . body)
     (and (identifier? #'name)
          (identifier? #'this)
          (identifier? #'function-object))
     (with-syntax ([function-object (prototype-fields-constructor-name
                                     (syntax-local-value #'function-object))])
       (check-params (syntax->list #'params))
       #'(es-property-set! (es-property function-object "prototype")
                           (es-property-name name)
                           (lambda (this . params) . body)))]))

(provide
 define-prototype
 define-prototype-generic
 (rename-out [define-prototype-generic define-generic])
 define-method
 define-prototype-method)

(define-match-expander object
  (lambda (stx)
    (syntax-case stx ()
      [(_ (key value) ...)
       #'(and (app (lambda (x) (es-property x key))
                   value) ...)]
      [(_ proto (value ...))
       (with-syntax ([(key ...) (prototype-fields-property-names
                                 (syntax-local-value #'proto))])
         #'(and (app (lambda (x) (es-property x key))
                     value) ...))])))

(provide object)
