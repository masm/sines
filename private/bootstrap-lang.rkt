#lang racket/base

(require
 racket/base
 racket/include
 racket/require-syntax
 (for-syntax racket/base)
 (for-syntax racket/path)
 (for-syntax syntax/stx)
 (for-syntax "../compiler/stx.rkt")
 (for-syntax "../backend.rkt")
 "../compiler/code-store.rkt"
 "../compiler/syntax.rkt")

(define-syntax (_#%module-begin stx)
  (if (eq? 'module-begin (syntax-local-context))
      (syntax-case stx ()
        [(_#%module-begin . forms)
         (let ([a (local-expand (syntax/loc stx (#%plain-module-begin . forms))
                                'module-begin '())])
           (let ([mappings (module-syntax->free-id-mappings a)])
             (let ([code (module-syntax->stx a)])
               (with-syntax ([code code]
                             [((id . string) ...) mappings]
                             [(_ body ...) a])
                 (syntax/loc stx (#%module-begin
                                  (mute body) ...
                                  (begin-for-syntax
                                    (add-free-id-mapping! #'id string) ...)
                                  (add-code code)))))))])
      (raise-syntax-error #f "allowed only around a module body" stx)))

(define-syntax (_#%top-interaction stx)
  (syntax-case stx ()
    [(_ . form)
     #`(begin
         (display '#,(local-expand #'form 'module '()))
         (newline)
         form)]))

(define-syntax _lambda
  (lambda (stx)
    (syntax-case stx ()
      [(_ (arg ... . rest) . body)
       (andmap identifier? (stx->list #'(arg ...)))
       #'(lambda (arg ... . rest) . body)]
      [(_ (arg ... . rest) . body)
       #'(letrec ([f (case-lambda* f (arg ...) () ()
                                   rest body)])
           f)])))

(define-syntax case-lambda*
  (syntax-rules ()
    [(case-lambda* f () (id ...) (clause ...)
                   rest body)
     (case-lambda clause ...
                  [(id ... . rest) . body])]
    [(case-lambda* f ([opt-id default-expr]
                      . rest-args)
                   (id ...) clauses rest body)
     (case-lambda* f rest-args (id ... opt-id)
                   ([(id ...)
                     (f id ... default-expr)]
                    . clauses)
                   rest body)]
    [(case-lambda* f (req-id . rest-args)
                   (id ...) clauses rest body)
     (case-lambda* f rest-args (id ... req-id)
                   clauses rest body)]))

(define-syntax _define
  (syntax-rules ()
    [(_ (id . args) . body)
     (_define id (_lambda args . body))]
    [(_ id value)
     (define id value)]))

(define-syntax (mute stx)
  (syntax-case stx ()
    [(_ form)
     (syntax-case #'form (#%plain-app
                          #%provide
                          #%require
                          begin
                          define-syntaxes
                          define-values
                          define-values-for-syntax
                          if
                          let-values
                          letrec-values
                          quote
                          set!
                          with-continuation-mark)
       [(#%plain-app . _)
        #'(void)]
       [(#%provide . x)
        #'form]
       [(#%require . x)
        #'form]
       [(begin form ...)
        #'(begin (mute form) ...)]
       [(define-syntaxes . _)
        #'form]
       [(define-values (id ...) value)
        (with-syntax ([(uniquifier ...) (map free-id-lookup (stx->list #'(id ...)))])
          #'(define-values (id ...) (values (make-sines-variable (make-module-id 'id uniquifier) 'value) ...)))]
       [(define-values-for-syntax . _)
        #'form]
       [(if . _)
        #'(void)]
       [(let-values . _)
        #'(void)]
       [(letrec-values . _)
        #'(void)]
       [(quote . _)
        #'(void)]
       [(set! . _)
        #'(void)]
       [(with-continuation-mark . _)
        #'(void)]
       [id
        (identifier? #'id)
        #'(void)]
       [_
        (error 'mute "unexpected form ~S" #'form)])]))

(begin-for-syntax
 (define (backend-file-string path file-str backend)
   (let loop ([backend backend])
     (let ([path (build-path path (regexp-replace #rx"_" file-str backend))])
       (cond [(file-exists? path) backend]
             [(string=? backend "chrome")        (loop "dom")]
             [(string=? backend "gecko")         (loop "dom")]
             [(string=? backend "gecko-ext")     (loop "gecko")]
             [(string=? backend "gecko-worker")  (loop "gecko")]
             [(string=? backend "gecko2")        (loop "dom")]
             [(string=? backend "gecko2-ext")    (loop "gecko2")]
             [(string=? backend "gecko2-worker") (loop "gecko2")]
             [else (error 'backend-include "file not found ~A" path)])))))

(define-syntax (backend-include stx)
  (syntax-case stx ()
    [(_ file)
     (string? (syntax-e #'file))
     (with-syntax ([_stx stx]
                   [new-file (regexp-replace #rx"_" (syntax-e #'file)
                                             (let ([filename (syntax-source stx)])
                                               (unless (path-string? filename)
                                                 (error 'backend-include "not a string ~A" filename))
                                               (backend-file-string (path-only filename) (syntax-e #'file) backend)))])
       (syntax/loc stx (include-at/relative-to _stx _stx new-file)))]))

;; (define-require-syntax (backend-specific stx )
;;   (syntax-case stx ()
;;     [(_ s ss ...)
;;      (string? (syntax-e #'s))
;;      (with-syntax ([new-s (regexp-replace #rx"_" (syntax-e #'s) backend)])
;;        #'(only-in new-s ss ...))]))

(provide
 (for-syntax (all-from-out racket/base))

 (rename-out (_#%module-begin #%module-begin)
             (_define define)
             (_lambda lambda))

 #%datum
 #%plain-app
 #%app

 and
 begin begin-for-syntax
 case-lambda
 else
 define-for-syntax define-values define-values-for-syntax
 define-syntax define-syntax-rule define-syntaxes
 if
 let let* let-values let*-values letrec letrec-values
 let-syntax letrec-syntax let-syntaxes letrec-syntaxes letrec-syntaxes+values
 or
 quote
 provide
 require
 set!
 with-continuation-mark

 unquote unquote-splicing

 only-in except-in prefix-in rename-in combine-in only-meta-in
 for-syntax for-template for-label for-meta
 lib file planet
 all-defined-out all-from-out rename-out except-out struct-out combine-out protect-out

 backend-include include)
