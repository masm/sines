#lang at-exp scheme/base

(require
 (for-syntax scheme/base)
 (for-syntax "stx.rkt")
 scheme/base
 scheme/match
 "syntax.rkt"
 (prefix-in s: "syntax2.rkt")
 (prefix-in c: "ecmascript.rkt")
 "store.rkt"
 "snippets.rkt"
 "backend.rkt"
 )

(define-struct sines-primitive (name transformer denotation referencial-transparent? side-effects-free?)
  #:property prop:procedure (lambda (t . args) (apply t args)))

(define-struct (sines-early-primitive sines-primitive) ())

(define primitives-hash (make-hash))

(define (lookup-sines-primitive id)
  (hash-ref primitives-hash id))

(define (add-primitive! id primitive)
  (hash-set! primitives-hash id primitive))

(define (sines-primitive-id? id)
  (hash-has-key? primitives-hash id))

(provide sines-primitive-id?
         lookup-sines-primitive
         (struct-out sines-primitive)
         (struct-out sines-early-primitive))

(define-syntax (define-untransformed-primitive stx)
  (syntax-case stx ()
    [(_ name value)
     #'(define-untransformed-primitive name value #:effect-free #f #:fixed-denotation #f #:transparent #f)]
    [(_ name value #:side-effect-free!)
     #'(define-untransformed-primitive name value #:effect-free #t #:fixed-denotation #f #:transparent #f)]
    [(_ name value #:referencial-transparent!)
     #'(define-untransformed-primitive name value #:effect-free #t #:fixed-denotation #f #:transparent #t)]
    [(_ name value #:fixed-denotation den)
     (identifier? #'den)
     #'(define-untransformed-primitive name value #:effect-free #t #:fixed-denotation den #:transparent #f)]
    [(_ name value #:side-effect-free! #:fixed-denotation den)
     (or (identifier? #'den) (boolean? (syntax-e #'den)))
     #'(define-untransformed-primitive name value #:effect-free #t #:fixed-denotation den #:transparent #f)]
    [(_ name value #:referencial-transparent! #:fixed-denotation den)
     (or (identifier? #'den) (boolean? (syntax-e #'den)))
     #'(define-untransformed-primitive name value #:effect-free #t #:fixed-denotation den #:transparent #t)]
    [(_ name value #:effect-free b  #:fixed-denotation den #:transparent t)
     (and (or (identifier? #'den) (boolean? (syntax-e #'den))) (boolean? (syntax-e #'b)) (boolean? (syntax-e #'t)))
     (with-syntax ([s (make-free-id-mapping! #'name)])
       #'(begin
           (define name
             (let ([id (make-module-id 'name s)])
               (let ([a (make-sines-primitive id value 'den t b)])
                 (add-primitive! id a)
                 a)))
           (begin-for-syntax
             (add-free-id-mapping! #'name s))))]))

(define-syntax (define-primitive stx)
  (syntax-case stx ()
    [(_ name value)
     #'(define-primitive name value #:effect-free #f #:fixed-denotation #f #:transparent #f)]
    [(_ name value #:side-effect-free!)
     #'(define-primitive name value #:effect-free #t #:fixed-denotation #f #:transparent #f)]
    [(_ name value #:referencial-transparent!)
     #'(define-primitive name value #:effect-free #t #:fixed-denotation #f #:transparent #t)]
    [(_ name value #:fixed-denotation den)
     (identifier? #'den)
     #'(define-primitive name value #:effect-free #t #:fixed-denotation den #:transparent #f)]
    [(_ name value #:side-effect-free! #:fixed-denotation den)
     (or (identifier? #'den) (boolean? (syntax-e #'den)))
     #'(define-primitive name value #:effect-free #t #:fixed-denotation den #:transparent #f)]
    [(_ name value #:referencial-transparent! #:fixed-denotation den)
     (or (identifier? #'den) (boolean? (syntax-e #'den)))
     #'(define-primitive name value #:effect-free #t #:fixed-denotation den #:transparent #t)]
    [(_ name value #:effect-free b #:fixed-denotation den  #:transparent t)
     (and (or (identifier? #'den) (boolean? (syntax-e #'den))) (boolean? (syntax-e #'b)) (boolean? (syntax-e #'t)))
     #'(define-untransformed-primitive name
         (lambda (transformer loc . args)
           (let ([l (map transformer args)])
             (with-handlers ([exn:fail? (lambda (v)
                                          (error (format "error applying primitive ~A: ~S" 'name
                                                         (exn-message v))))])
               (apply value loc l))))
         #:effect-free b #:fixed-denotation den #:transparent t)]))

(provide define-primitive)

(define-syntax (define-early-primitive stx)
  (syntax-case stx ()
    [(_ name value)
     (with-syntax ([s (make-free-id-mapping! #'name)])
       #'(begin
           (define name
             (let ([id (make-module-id 'name s)])
               (let ([a (make-sines-early-primitive id value #f #f #f)])
                 (add-primitive! id a)
                 a)))
           (begin-for-syntax
             (add-free-id-mapping! #'name s))))]))

;;; Bootstrap primitives

(define-primitive store-continuation-exception-constructor-procedure
  (lambda (loc)
    (let ()
      (c:fn (list)
            (c:empty))))
  #:fixed-denotation procedure)

(define-primitive switch-continuation-exception-constructor-procedure
  (lambda (loc)
    (let ([b 'za])
     (c:fn (list b)
           (c:empty)
           (c:= (c:field (c:this) abort-continuation?-property-name) b))))
  #:fixed-denotation procedure)

(define-primitive tail-trampoline-constructor-procedure
  (lambda (loc)
    (let ([proc 'za] [args 'zb])
      (c:fn (list proc args)
            (c:= (c:field (c:this) tail-trampoline-proc-property-name) proc)
            (c:= (c:field (c:this) tail-trampoline-args-property-name) args))))
  #:fixed-denotation procedure)

(define-primitive continuation-trampoline-procedure
  (lambda (loc continuation-index-var
               continuation-state-var
               current-toplevel-var
               stack-traces-var
               store-continuation-exception-var
               switch-continuation-exception-var)
    (let ([original-exception  original-exception-property-name]
          [abort-continuation? abort-continuation?-property-name])
      @js[#:loc loc]{
        function () {
            while (true)
               try {
                   return @current-toplevel-var();
               } catch (zq) {
                   if (zq instanceof @store-continuation-exception-var)
                       @continuation-index-var = @|continuation-state-var|.length;
                   else if (zq instanceof @switch-continuation-exception-var) {
                       if (! zq["@abort-continuation?"])
                           break;
                   } else
                       throw zq;
               }}}))
  #:fixed-denotation procedure)

(define-primitive tail-trampoline-restart-procedure
  (lambda (loc continuation-index-var
               continuation-state-var
               tail-object-var
               store-continuation-exception-var
               switch-continuation-exception-var
               tail-trampoline-var)
    (let ([trampoline 'za]
          [proc-property tail-trampoline-proc-property-name]
          [args-property tail-trampoline-args-property-name])
      (c:fn (list trampoline)
            (recover-continuation-code continuation-index-var
                                       continuation-state-var
                                       (list trampoline))
            (c:try
             (c:block
              (c:do-while (c:instance-of trampoline tail-trampoline-var)
                          (c:= trampoline (c:call (c:field (c:field trampoline proc-property)
                                                           "apply")
                                                  tail-object-var
                                                  (c:field trampoline args-property))))
              (c:return trampoline))
             'zq
             (c:block
              (create-stack-code continuation-state-var
                                 store-continuation-exception-var
                                 'zq
                                 (c:array trampoline))
              (c:throw 'zq))))
      ;; @js[#:loc loc]{
      ;;   function (@trampoline) {
      ;;       @(recover-continuation-code continuation-index-var continuation-state-var (list trampoline))
      ;;       try {
      ;;           do {
      ;;               @trampline = @|trampoline|[@proc-property].apply(@tail-object-var, @|trampoline|[@args-property]);
      ;;           } while (@trampoline instanceof @tail-trampoline);
      ;;           return @trampoline;
      ;;       } catch (zq) {
      ;;           @(create-stack-code continuation-state-var
      ;;                               store-continuation-exception-var
      ;;                               'zq
      ;;                               (c:array trampoline))
      ;;           throw zq;
      ;;       }
      ;;   }}
      ))
  #:fixed-denotation procedure)

(define-primitive arguments->rest-parameter-procedure
  (lambda (loc)
    (let ([value 'zb] [len 'zc] [acc 'zd] [i 'ze])
      @js[#:loc loc]{
        function (@value, @len) {
            var @acc = null;
            for (var @i = @|value|.length - 1;
                 @len <= @i;
                 --@i)
                @acc = { @car-property-name : @|value|[@i], @cdr-property-name : @acc };
            return @acc;
        }}))
  #:fixed-denotation procedure)

(provide store-continuation-exception-constructor-procedure
         switch-continuation-exception-constructor-procedure
         tail-trampoline-constructor-procedure
         continuation-trampoline-procedure
         tail-trampoline-restart-procedure
         arguments->rest-parameter-procedure)

;;;; ECMAScript syntax

(define-early-primitive es-property-name
  (lambda (loc name)
    (match name
      [(s:global-ref id) (s:literal (property-name id) loc)]
      [_ (error "es-property-name primitive: not matched" name)])))

(define-early-primitive es-property-name!
  (lambda (loc name property-name)
    (match name
      [(s:global-ref id)
       (match property-name
         [(s:literal value)
          (set-find-property-name! id value)
          (s:literal value loc)]
         [_ (error "es-property-name! primitive (2): not matched" name)])]
      [_ (error "es-property-name! primitive: not matched" name)])))

(define-untransformed-primitive es-global
  (lambda (transformer loc name)
    (match name
      [(s:literal value)
       (c:var-ref (string->symbol value) #:loc loc)]
      [_ (error "es-global primitive: not matched" name)]))
  #:side-effect-free! #:fixed-denotation #t)

(define-primitive es-property
  (lambda (loc obj property)
    ;; @js[#:loc loc]{@|obj|[@property]}
    (c:field obj property
             #:loc loc))
  #:side-effect-free!)

(define-primitive es-property-set!
  (lambda (loc obj property value)
    ;; @js[#:loc loc]{@|obj|[@property] = @value}
    (c:= (c:field obj property)
         value
         #:loc loc)))

(define-primitive es-in?
  (lambda (loc property obj)
    (c:in property obj #:loc loc))
  #:referencial-transparent!)

(define-primitive es-instance?
  (lambda (loc obj proc)
    ;; @js[#:loc loc]{@obj instanceof @proc}
    (c:instance-of obj proc #:loc loc))
  #:referencial-transparent!)

(define-primitive es-call
  (lambda (loc proc . args)
    (apply c:call
           ;; #:loc loc
           proc
           args)))

(define-primitive es-call-property
  (lambda (loc obj property . args)
    (apply c:call
           ;; #:loc loc
           (c:field obj property)
           args)))

(define-primitive es-typeof
  (lambda (loc obj)
    @js[#:loc loc]{typeof @obj})
  #:referencial-transparent!)

(define-primitive es-new
  (lambda (loc obj . args)
    (apply c:new
           ;; #:loc loc
           obj
           args)))

(define-primitive es-argument
  (lambda (loc index)
    @js[#:loc loc]{arguments[@index]})
  #:side-effect-free!)

(define-primitive es-arguments
  (lambda (loc)
    @js[#:loc loc]{arguments})
  #:side-effect-free!)

(define-primitive es-undefined?
  (lambda (loc obj)
    @js[#:loc loc]{undefined === @obj})
  #:referencial-transparent!)

(define-primitive es-apply-to/list
  (lambda (loc proc object arg . args)
    (match (cons arg args)
      [(list init ... last)
       (let ([arg 'za] [acc 'zb])
         @js[#:loc loc]{
           @|proc|.apply(@object,
               (function (@arg) {
                       var @acc = @apply[c:array init];
                       for (;
                            @arg !== null;
                            @arg = @|arg|["@cdr-property-name"])
                           @|acc|.push(@|arg|["@car-property-name"]);
                       return @acc;
                })(@last))})])))

(define-primitive es-this
  (lambda (loc)
    @js[#:loc loc]{this})
  #:side-effect-free!)

(define-primitive es-object
  (lambda (loc . args)
    (apply c:object
           (let loop [(args args)]
             (cond [(null? args)
                    '()]
                   [(null? (cdr args))
                    (error 'es-object "must be used with an even number of arguments")]
                   [else
                    (cons (cons (car args) (cadr args))
                          (loop (cddr args)))]))))
  #:referencial-transparent!)

(define-primitive es-array
  (lambda (loc . args)
    (apply c:array args))
  #:referencial-transparent!)

(define-primitive es-delete
  (lambda (loc obj prop)
    @js[#:loc loc]{delete @|obj|[@prop]}))

(define-primitive es-throw
  (lambda (loc obj)
    (c:call (c:fn '() (c:throw obj))
            #:loc loc)))

;; (define-primitive es-throw
;;   (lambda (loc obj)
;;     @js[#:loc loc]{(function() {throw @obj})()}))

(provide es-global
         es-property-name
         es-property-name!
         es-property
         es-property-set!
         es-in?
         es-instance?
         es-new
         es-argument
         es-arguments
         es-call
         es-call-property
         es-typeof
         es-undefined?
         es-this
         es-apply-to/list
         es-object
         es-array

         es-delete
         es-throw
         )
