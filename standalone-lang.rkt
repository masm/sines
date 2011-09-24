#lang scheme/base

(require scheme/base
         scheme/include
         (for-syntax scheme/base)
         (for-syntax "stx.rkt")
         "code-store.rkt"
         "compiler.rkt"
         "syntax.rkt")

(define-syntax (_#%module-begin stx)
  (if (eq? 'module-begin (syntax-local-context))
      (syntax-case stx ()
        [(_ . forms)
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
                                  (let ([s (modules->javascript-string
                                            (reverse (cons code (code-list))))])
                                    (display s)
                                    ;; (with-new-runtime
                                    ;;  (with-new-context
                                    ;;   (with-new-global-object
                                    ;;    (run-script s))))
                                    )))))))])
      (raise-syntax-error #f "allowed only around a module body" stx)))

(include "partial-lang.rkt")
