#lang scheme/base

(require scheme/base
         (for-syntax scheme/base)
         scheme/match
         "syntax.rkt")

(define-match-expander literal
  (syntax-rules ()
    [(_ value loc)
     (struct literal-stx [loc value])]
    [(_ value)
     (struct literal-stx [_ value])]
    [(_)
     (struct literal-stx _)])
  (syntax-rules ()
    [(_ value loc)
     (make-literal-stx loc value)]
    [(_ value)
     (make-literal-stx #f value)]))

(define-match-expander lexical-ref
  (syntax-rules ()
    [(_ id loc)
     (struct lexical-ref-stx [loc id])]
    [(_ id)
     (struct lexical-ref-stx [_ id])]
    [(_)
     (struct lexical-ref-stx _)])
  (syntax-rules ()
    [(_ id loc)
     (make-lexical-ref-stx loc id)]
    [(_ id)
     (make-lexical-ref-stx #f id)]))

(define-match-expander global-ref
  (syntax-rules ()
    [(_ id loc)
     (struct global-ref-stx [loc id])]
    [(_ id)
     (struct global-ref-stx [_ id])]
    [(_)
     (struct global-ref-stx _)])
  (syntax-rules ()
    [(_ id loc)
     (make-global-ref-stx loc id)]
    [(_ id)
     (make-global-ref-stx #f id)]))

(define-match-expander primapp
  (syntax-rules ()
    [(_ op args loc)
     (struct primapp-stx [loc op args])]
    [(_ op args)
     (struct primapp-stx [_ op args])]
    [(_)
     (struct primapp-stx _)])
  (syntax-rules ()
    [(_ op args loc)
     (make-primapp-stx loc op args)]
    [(_ op args)
     (make-primapp-stx #f op args)]))

(define-match-expander app
  (syntax-rules ()
    [(_ op args loc)
     (struct app-stx [loc op args])]
    [(_ op args)
     (struct app-stx [_ op args])]
    [(_)
     (struct app-stx _)])
  (syntax-rules ()
    [(_ op args loc)
     (make-app-stx loc op args)]
    [(_ op args)
     (make-app-stx #f op args)]))

(define-match-expander _begin
  (syntax-rules ()
    [(_ body loc)
     (struct begin-stx [loc body])]
    [(_ body)
     (struct begin-stx [_ body])]
    [(_)
     (struct begin-stx _)])
  (syntax-rules ()
    [(_ body loc)
     (make-begin-stx loc body)]
    [(_ body)
     (make-begin-stx #f body)]))

(define-match-expander begin/var
  (syntax-rules ()
    [(_ ids body loc)
     (struct begin/var-stx [loc ids body])]
    [(_ ids body)
     (struct begin/var-stx [_ ids body])]
    [(_)
     (struct begin/var-stx _)])
  (syntax-rules ()
    [(_ ids body loc)
     (make-begin/var-stx loc ids body)]
    [(_ ids body)
     (make-begin/var-stx #f ids body)]))

(define-match-expander begin/const/var
  (syntax-rules ()
    [(_ const-ids const-values ids body loc)
     (struct begin/const/var-stx [loc const-ids const-values ids body])]
    [(_ const-ids const-values ids body)
     (struct begin/const/var-stx [_ const-ids const-values ids body])]
    [(_)
     (struct begin/var-stx _)])
  (syntax-rules ()
    [(_ const-ids const-values ids body loc)
     (make-begin/const/var-stx loc const-ids const-values ids body)]
    [(_ const-ids const-values ids body)
     (make-begin/const/var-stx #f const-ids const-values ids body)]))

(define-match-expander blocks
  (syntax-rules ()
    [(_ ids blocks loc)
     (struct blocks-stx [loc ids blocks])]
    [(_ ids blocks)
     (struct blocks-stx [_ ids blocks])]
    [(_)
     (struct begin/var-stx _)])
  (syntax-rules ()
    [(_ ids blocks loc)
     (make-blocks-stx loc ids blocks)]
    [(_ ids blocks)
     (make-blocks-stx #f ids blocks)]))

(define-match-expander block
  (syntax-rules ()
    [(_ frames body loc)
     (struct block-stx [loc frames body])]
    [(_ frames body)
     (struct block-stx [_ frames body])]
    [(_)
     (struct begin/var-stx _)])
  (syntax-rules ()
    [(_ frames body loc)
     (make-block-stx loc frames body)]
    [(_ frames body)
     (make-block-stx #f frames body)]))

(define-match-expander _define-values
  (syntax-rules ()
    [(_ ids value loc)
     (struct define-values-stx [loc ids value])]
    [(_ ids value)
     (struct define-values-stx [_ ids value])]
    [(_)
     (struct define-values-stx _)])
  (syntax-rules ()
    [(_ ids value loc)
     (make-define-values-stx loc ids value)]
    [(_ ids value)
     (make-define-values-stx #f ids value)]))

(define-match-expander _if
  (syntax-rules ()
    [(_ test then else loc)
     (struct if-stx [loc test then else])]
    [(_ test then else)
     (struct if-stx [_ test then else])]
    [(_)
     (struct if-stx _)])
  (syntax-rules ()
    [(_ test then else loc)
     (make-if-stx loc test then else)]
    [(_ test then else)
     (make-if-stx #f test then else)]))

(define-match-expander if-true
  (syntax-rules ()
    [(_ test then loc)
     (struct if-true-stx [loc test then])]
    [(_ test then)
     (struct if-true-stx [_ test then])]
    [(_)
     (struct if-true-stx _)])
  (syntax-rules ()
    [(_ test then loc)
     (make-if-true-stx loc test then)]
    [(_ test then)
     (make-if-true-stx #f test then)]))

(define-match-expander if-false
  (syntax-rules ()
    [(_ test then loc)
     (struct if-false-stx [loc test then])]
    [(_ test then)
     (struct if-false-stx [_ test then])]
    [(_)
     (struct if-false-stx _)])
  (syntax-rules ()
    [(_ test then loc)
     (make-if-false-stx loc test then)]
    [(_ test then)
     (make-if-false-stx #f test then)]))

(define-match-expander _lambda
  (syntax-rules ()
    [(_ ids rest-id body loc)
     (struct lambda-stx [loc ids rest-id body])]
    [(_ ids rest-id body)
     (struct lambda-stx [_ ids rest-id body])]
    [(_)
     (struct lambda-stx _)])
  (syntax-rules ()
    [(_ ids rest-id body loc)
     (make-lambda-stx loc ids rest-id body)]
    [(_ ids rest-id body)
     (make-lambda-stx #f ids rest-id body)]))

(define-match-expander _case-lambda
  (syntax-rules ()
    [(_ clauses loc)
     (struct case-lambda-stx [loc clauses])]
    [(_ clauses)
     (struct case-lambda-stx [_ clauses])]
    [(_)
     (struct case-lambda-stx _)])
  (syntax-rules ()
    [(_ clauses loc)
     (make-case-lambda-stx loc clauses)]
    [(_ clauses)
     (make-case-lambda-stx #f clauses)]))

(define-match-expander _case-lambda-clause
  (syntax-rules ()
    [(_ ids rest-id body loc)
     (struct case-lambda-clause-stx [loc ids rest-id body])]
    [(_ ids rest-id body)
     (struct case-lambda-clause-stx [_ ids rest-id body])]
    [(_)
     (struct case-lambda-clause-stx _)])
  (syntax-rules ()
    [(_ ids rest-id body loc)
     (make-case-lambda-clause-stx loc ids rest-id body)]
    [(_ ids rest-id body)
     (make-case-lambda-clause-stx #f ids rest-id body)]))

(define-match-expander dispatch-lambda
  (syntax-rules ()
    [(_ procs loc)
     (struct dispatch-lambda-stx [loc procs])]
    [(_ procs)
     (struct dispatch-lambda-stx [_ procs])]
    [(_)
     (struct dispatch-lambda-stx _)])
  (syntax-rules ()
    [(_ procs loc)
     (make-dispatch-lambda-stx loc procs)]
    [(_ procs)
     (make-dispatch-lambda-stx #f procs)]))

(define-match-expander _let-values
  (syntax-rules ()
    [(_ ids values body loc)
     (struct let-values-stx [loc ids values body])]
    [(_ ids values body)
     (struct let-values-stx [_ ids values body])]
    [(_)
     (struct let-values-stx _)])
  (syntax-rules ()
    [(_ ids values body loc)
     (make-let-values-stx loc ids values body)]
    [(_ ids values body)
     (make-let-values-stx #f ids values body)]))

(define-match-expander _letrec-values
  (syntax-rules ()
    [(_ ids values body loc)
     (struct letrec-values-stx [loc ids values body])]
    [(_ ids values body)
     (struct letrec-values-stx [_ ids values body])]
    [(_)
     (struct letrec-values-stx _)])
  (syntax-rules ()
    [(_ ids values body loc)
     (make-letrec-values-stx loc ids values body)]
    [(_ ids values body)
     (make-letrec-values-stx #f ids values body)]))

(define-match-expander fix
  (syntax-rules ()
    [(_ id values body loc)
     (struct fix-stx [loc id values body])]
    [(_ id values body)
     (struct fix-stx [_ id values body])]
    [(_)
     (struct fix-stx _)])
  (syntax-rules ()
    [(_ id values body loc)
     (make-fix-stx loc id values body)]
    [(_ id values body)
     (make-fix-stx #f id values body)]))

(define-match-expander _set!
  (syntax-rules ()
    [(_ var value loc)
     (struct set!-stx [loc var value])]
    [(_ var value)
     (struct set!-stx [_ var value])]
    [(_)
     (struct set!-stx _)])
  (syntax-rules ()
    [(_ var value loc)
     (make-set!-stx loc var value)]
    [(_ var value)
     (make-set!-stx #f var value)]))

(define-match-expander wcm
  (syntax-rules ()
    [(_ key value expr loc)
     (struct wcm-stx [loc key value expr])]
    [(_ key value expr)
     (struct wcm-stx [_ key value expr])]
    [(_)
     (struct wcm-stx _)])
  (syntax-rules ()
    [(_ key value expr loc)
     (make-wcm-stx loc key value expr)]
    [(_ key value expr)
     (make-wcm-stx #f key value expr)]))

(define-match-expander wcmf
  (syntax-rules ()
    [(_ ids expr loc)
     (struct wcmf-stx [loc ids expr])]
    [(_ ids expr)
     (struct wcmf-stx [_ ids expr])]
    [(_)
     (struct wcmf-stx _)])
  (syntax-rules ()
    [(_ ids expr loc)
     (make-wcmf-stx loc ids expr)]
    [(_ ids expr)
     (make-wcmf-stx #f ids expr)]))

(define-match-expander wcmtf
  (syntax-rules ()
    [(_ ids expr loc)
     (struct wcmtf-stx [loc ids expr])]
    [(_ ids expr)
     (struct wcmtf-stx [_ ids expr])]
    [(_)
     (struct wcmtf-stx _)])
  (syntax-rules ()
    [(_ ids expr loc)
     (make-wcmtf-stx loc ids expr)]
    [(_ ids expr)
     (make-wcmtf-stx #f ids expr)]))

(define-match-expander wcm/body
  (syntax-rules ()
    [(_ key value body loc)
     (struct wcm/body-stx [loc key value body])]
    [(_ key value body)
     (struct wcm/body-stx [_ key value body])]
    [(_)
     (struct wcm/body-stx _)])
  (syntax-rules ()
    [(_ key value body loc)
     (make-wcm/body-stx loc key value body)]
    [(_ key value body)
     (make-wcm/body-stx #f key value body)]))

(define-match-expander lexical-init
  (syntax-rules ()
    [(_ id value loc)
     (struct lexical-init-stx [loc id value])]
    [(_ id value)
     (struct lexical-init-stx [_ id value])]
    [(_)
     (struct lexical-init-stx _)])
  (syntax-rules ()
    [(_ id value loc)
     (make-lexical-init-stx loc id value)]
    [(_ id value)
     (make-lexical-init-stx #f id value)]))

(define-match-expander global-init
  (syntax-rules ()
    [(_ id value loc)
     (struct global-init-stx [loc id value])]
    [(_ id value)
     (struct global-init-stx [_ id value])]
    [(_)
     (struct global-init-stx _)])
  (syntax-rules ()
    [(_ id value loc)
     (make-global-init-stx loc id value)]
    [(_ id value)
     (make-global-init-stx #f id value)]))

(define-match-expander program
  (syntax-rules ()
    [(_ body loc)
     (struct program-stx [loc body])]
    [(_ body)
     (struct program-stx [_ body])]
    [(_)
     (struct program-stx _)])
  (syntax-rules ()
    [(_ body loc)
     (make-program-stx loc body)]
    [(_ body)
     (make-program-stx #f body)]))

(define-match-expander program/var
  (syntax-rules ()
    [(_ global-ids lexical-ids body loc)
     (struct program/var-stx [loc global-ids lexical-ids body])]
    [(_ global-ids lexical-ids body)
     (struct program/var-stx [_ global-ids lexical-ids body])]
    [(_)
     (struct program/var-stx _)])
  (syntax-rules ()
    [(_ global-ids lexical-ids body loc)
     (make-program/var-stx loc global-ids lexical-ids body)]
    [(_ global-ids lexical-ids body)
     (make-program/var-stx #f global-ids lexical-ids body)]))

(define-match-expander program/const/var
  (syntax-rules ()
    [(_ const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body loc)
     (struct program/const/var-stx [loc const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body])]
    [(_ const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body)
     (struct program/const/var-stx [_ const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body])]
    [(_) (struct program/var-stx _)])
  (syntax-rules ()
    [(_ const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body loc)
     (make-program/const/var-stx loc const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body)]
    [(_ const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body)
     (make-program/const/var-stx #f const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body)]))

(define-match-expander loop
  (syntax-rules ()
    [(_ ids values body loc)
     (struct loop-stx [loc ids values body])]
    [(_ ids values body)
     (struct loop-stx [_ ids values body])]
    [(_)
     (struct loop-stx _)])
  (syntax-rules ()
    [(_ ids values body loc)
     (make-loop-stx loc ids values body)]
    [(_ ids values body)
     (make-loop-stx #f ids values body)]))

(define-match-expander iterate
  (syntax-rules ()
    [(_ args loc)
     (struct iterate-stx [loc args])]
    [(_ args)
     (struct iterate-stx [_ args])]
    [(_)
     (struct iterate-stx _)])
  (syntax-rules ()
    [(_ args loc)
     (make-iterate-stx loc args)]
    [(_ args)
     (make-iterate-stx #f args)]))

(define-match-expander tail
  (syntax-rules ()
    [(_ app loc)
     (struct tail-stx [loc app])]
    [(_ app)
     (struct tail-stx [_ app])]
    [(_)
     (struct tail-stx _)])
  (syntax-rules ()
    [(_ app loc)
     (make-tail-stx loc app)]
    [(_ app)
     (make-tail-stx #f app)]))

(define-match-expander non-tail
  (syntax-rules ()
    [(_ app loc)
     (struct non-tail-stx [loc app])]
    [(_ app)
     (struct non-tail-stx [_ app])]
    [(_)
     (struct non-tail-stx _)])
  (lambda (stx)
    (syntax-case stx ()
      [(_ app loc)
       #'(make-non-tail-stx loc app)]
      [(_ app)
       #'(make-non-tail-stx #f app)]
      [non-tail
       (identifier? #'non-tail)
       #'(lambda (app)
           (non-tail app))])))

(provide literal lexical-ref global-ref
         primapp app
         (rename-out (_begin begin))
         begin/var begin/const/var blocks block
         (rename-out (_define-values define-values))
         (rename-out (_if if))
         if-true if-false
         (rename-out (_let-values let-values))
         (rename-out (_letrec-values letrec-values))
         fix
         (rename-out (_lambda lambda))
         (rename-out (_case-lambda case-lambda))
         (rename-out (_case-lambda-clause case-lambda-clause))
         dispatch-lambda
         (rename-out (_set! set!))
         wcm wcmf wcmtf wcm/body
         lexical-init global-init
         program program/var program/const/var
         loop iterate
         tail non-tail)

#;
(begin
  (define-struct (module-stx stx)        (body)                  #:prefab))
