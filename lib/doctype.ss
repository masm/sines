#lang s-exp "../sines-lang.rkt"

(require
 (for-syntax scheme/base)
 (for-syntax scheme/list)
 (for-syntax scheme/match)
 (for-syntax syntax/stx)
 (for-syntax "doctype-info.rkt")
 "../es/primitives.ss")

(begin-for-syntax
  (define (stx-string? stx)
    (and (syntax? stx)
         (string? (syntax-e stx)))))

(define type-parents (sdict))

(define-syntax define-doctype
  (lambda (stx)
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
               [id
                (identifier? #'id)
                #'id]))
           (syntax->list slots-stx)))
    (define (accessor-names name-stx slots-stx)
      (define (accessor-name name-stx slot-name-stx)
        (datum->syntax name-stx
                       (string->symbol (string-append (symbol->string (syntax->datum name-stx))
                                                      "-"
                                                      (symbol->string (syntax->datum slot-name-stx))))))
      (map (lambda (stx)
             (accessor-name name-stx stx))
           (slot-names slots-stx)))
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
                      [id
                       (identifier? #'id)
                       name]))
                  (syntax->list slots-stx)
                  (syntax->list accessor-names-stx)))
    (define (setter-names slots-stx accessor-names-stx)
      (filter-map (lambda (slot-spec name)
                    (syntax-case slot-spec ()
                      [(id #:mutable . _)   name]
                      [(id #:string-name _ #:mutable) name]
                      [(id #:string-name _) #f]
                      [id
                       (identifier? #'id)
                       #f]))
                  (syntax->list slots-stx)
                  (syntax->list accessor-names-stx)))
    (define (getter-string-names slots-stx)
      (filter-map (lambda (slot-spec)
                    (syntax-case slot-spec ()
                      [(_ #:mutable #:string-name name) #f]
                      [(_ #:mutable) #f]
                      [(_ #:string-name name #:mutable) #f]
                      [(_ #:string-name name) #'name]
                      [id
                       (identifier? #'id)
                       #'#f]))
                  (syntax->list slots-stx)))
    (define (setter-string-names slots-stx)
      (filter-map (lambda (slot-spec)
                    (syntax-case slot-spec ()
                      [(_ #:mutable #:string-name name) #'name]
                      [(_ #:mutable) #'#f]
                      [(_ #:string-name name #:mutable) #'name]
                      [(_ #:string-name _) #f]
                      [id
                       (identifier? #'id)
                       #f]))
                  (syntax->list slots-stx)))
    (define (getter-and-setter-string-names slots-stx accessor-names-stx)
      (map (lambda (slot-spec accessor-name)
             (syntax-case slot-spec ()
               [(_ #:mutable #:string-name name) #'name]
               [(_ #:mutable) #`(es-property-name #,accessor-name)]
               [(_ #:string-name name #:mutable) #'name]
               [(_ #:string-name name) #'name]
               [id
                (identifier? #'id)
                #`(es-property-name #,accessor-name)]))
           (syntax->list slots-stx)
           (syntax->list accessor-names-stx)))
    (syntax-case stx ()
      [(_ name type-str (slot ...))
       (and (identifier? #'name)
            (stx-string? #'type-str))
       (with-syntax ([(accessor-name ...) (accessor-names #'name #'(slot ...))])
         (with-syntax ([maker-name (maker-name #'name)]
                       [predicate-name (predicate-name #'name)]
                       [(param-name ...) (constructor-params #'(slot ...))]
                       [(getter ...) (getter-names #'(slot ...) #'(accessor-name ...))]
                       [(setter ...) (setter-names #'(slot ...) #'(accessor-name ...))]
                       [(property-name ...) (getter-and-setter-string-names #'(slot ...) #'(accessor-name ...))]
                       [(getter-name ...) (getter-string-names #'(slot ...))]
                       [(setter-name ...) (setter-string-names #'(slot ...))])
           (with-syntax ([((ppp ...) ...) #'((property-name param-name) ...)])
             #'(begin
                 (define-syntax name
                   (cons type-str
                         (cons (list #'param-name ...)
                               (list #'property-name ...))))
                 (define-getter (getter name) getter-name) ...
                 (define-getter+setter (setter name) setter-name) ...
                 (define (maker-name param-name ...)
                   (es-object "type" type-str ppp ... ...))
                 (define (predicate-name obj)
                   (and (es-object? obj)
                        (sdict-key? obj "type")
                        (string=? type-str (es-property obj "type"))))))))]
      [(_ (name parent) type-str (slot ...))
       (and (identifier? #'name) (identifier? #'parent) (stx-string? #'type-str))
       (let ([parent-fields (syntax-local-value #'parent)])
         (with-syntax ([(accessor-name ...) (accessor-names #'name #'(slot ...))]
                       [parent-type-str (car parent-fields)]
                       [(parent-param-name ...) (cadr parent-fields)]
                       [(parent-property-name ...) (cddr parent-fields)])
           (with-syntax ([maker-name (maker-name #'name)]
                         [predicate-name (predicate-name #'name)]
                         [(param-name ...) (constructor-params #'(slot ...))]
                         [(getter ...) (getter-names #'(slot ...) #'(accessor-name ...))]
                         [(setter ...) (setter-names #'(slot ...) #'(accessor-name ...))]
                         [(property-name ...) (getter-and-setter-string-names #'(slot ...) #'(accessor-name ...))]
                         [(getter-name ...) (getter-string-names #'(slot ...))]
                         [(setter-name ...) (setter-string-names #'(slot ...))])
             (with-syntax ([((ppp ...) ...) #'((parent-property-name parent-param-name) ... (property-name param-name) ...)])
               #'(begin
                   (define-syntax name
                     (cons type-str
                           (cons (list #'parent-param-name ... #'param-name ...)
                                 (list #'parent-property-name ... #'property-name ...))))
                   (define-getter (getter name) getter-name) ...
                   (define-getter+setter (setter name) setter-name) ...
                   (define (maker-name parent-param-name ... param-name ...)
                     (es-object "type" type-str ppp ... ...))
                   (define (predicate-name obj)
                     (and (es-object? obj)
                          (sdict-key? obj "type")
                          (string=? type-str (es-property obj "type"))))
                   (sdict-set! type-parents type-str parent-type-str))))))])))

(provide define-doctype)

;;;

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

(define-syntax define-doctype-generic
  (lambda (stx)
    (syntax-case stx ()
      [(_ (name object param ...))
       (and (identifier? #'name)
            (identifier? #'object))
       (let ([params (syntax->list #'(param ...))])
         (check-params params)
         (with-syntax ([(param-name ...) (params-names params)])
           #'(begin
               (define d (sdict))
               (define (new-name object param ...)
                 ((lookup-doctype-method d (sdict-ref object "type")) object param-name ...))
               (define-syntax name
                 (doctype-info #'d #'new-name)))))]
      [(_ (name object param ... . last-param))
       (and (identifier? #'name)
            (identifier? #'object)
            (identifier? #'last-param))
       (let ([params (syntax->list #'(param ...))])
         (check-params (syntax->list #'(param ...)))
         (with-syntax ([(param-name ...) (params-names params)])
           #'(begin
               (define d (sdict))
               (define (new-name object param ... . last-param)
                 (apply (lookup-doctype-method d (sdict-ref object "type")) object param-name ... last-param))
               (define-syntax name
                 (doctype-info #'d #'new-name)))))])))

(define (lookup-doctype-method dict type-str)
  (cond [(sdict-key? dict type-str)
         (sdict-ref dict type-str)]
        [(sdict-key? type-parents type-str)
         (let loop ([ts (sdict-ref type-parents type-str)])
           (cond [(sdict-key? dict ts)
                  (sdict-ref dict ts)]
                 [(sdict-key? type-parents ts)
                  (loop (sdict-ref type-parents ts))]
                 [else (error 'lookup-doctype-method "no doctype method defined for type" type-str)]))]
        [else (error 'lookup-doctype-method "no doctype method defined for type" type-str)]))

(define-syntax (define-doctype-method stx)
  (syntax-case stx ()
    [(_ (name [this doctype] . params) . body)
     (and (identifier? #'name)
          (identifier? #'this)
          (identifier? #'doctype))
     (with-syntax ([type-str (car (syntax-local-value #'doctype))]
                   [d (let-values ([(info _) (syntax-local-value/immediate #'name)])
                        (doctype-info-dict info))])
       (check-params (syntax->list #'params))
       #'(es-property-set! d type-str (lambda (this . params) . body)))]))

(provide
 define-doctype-generic
 define-doctype-method)
