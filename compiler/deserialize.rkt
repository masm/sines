#lang scheme/base

(require
 scheme/base
 scheme/match
 "syntax.rkt"
 (prefix-in s: "syntax2.rkt")
 "primitives.rkt")

(define (global->sexp id)
  ;;`(global ,(module-id-name id) ,(module-id-source id))
  (module-id-name id))

(define (lexical->sexp id)
  ;; `(lexical ,(id-name id) ,(id-binding id))
  (id-name id))

(define (code->sexp node)
  (match node
    [(s:literal value)   value]
    [(s:lexical-ref id)  (lexical->sexp id)]
    [(s:global-ref id)   (global->sexp id)]
    [(s:primapp transformer args) `(primapp ,(module-id-name (sines-primitive-name transformer)) ,@(map code->sexp args))]
    [(s:app op args) `(,(code->sexp op) ,@(map code->sexp args))]
    [(s:begin body) `(begin ,@(map code->sexp body))]
    [(s:blocks ids blocks)
     `(blocks
        (ids ,@(map lexical->sexp ids))
        ,@(map code->sexp blocks))]
    [(s:block frames body)
     `(block
        (frames ,@(map (lambda (ids)
                         (map lexical->sexp ids))
                       frames))
        ,@(map code->sexp body))]
    [(s:lambda ids rest-id body)
     (if rest-id
         `(lambda (,@(map lexical->sexp ids) . ,(lexical->sexp rest-id))
            ,(code->sexp body))
         `(lambda (,@(map lexical->sexp ids))
            ,(code->sexp body)))]
    [(s:dispatch-lambda procs) `(dispatch-lambda ,@(map code->sexp procs))]
    [(s:case-lambda clauses) `(case-lambda ,@(map (lambda (clause)
                                                    (match clause
                                                      [(s:case-lambda-clause ids rest-id body)
                                                       (if rest-id
                                                           `[(,@(map lexical->sexp ids) . ,(lexical->sexp rest-id)) ,(code->sexp body)]
                                                           `[,(map lexical->sexp ids) ,(code->sexp body)])]))
                                                  clauses))]
    [(s:if test then else) `(if ,(code->sexp test) ,(code->sexp then) ,(code->sexp else))]
    [(s:if-true test then) `(if-true ,(code->sexp test) ,(code->sexp then))]
    [(s:if-false test then) `(if-false ,(code->sexp test) ,(code->sexp then))]
    [(s:let-values ids values body)
     (if (= 1 (length ids))
         `(let ,(map (lambda (ids value)
                       `(,(lexical->sexp (car ids)) ,(code->sexp value)))
                     ids values)
            ,(code->sexp body))
         `(let-values ,(map (lambda (ids value)
                              `(,(map lexical->sexp ids) ,(code->sexp value)))
                            ids values)
            ,(code->sexp body)))]
    [(s:letrec-values ids values body)
     (if (= 1 (length ids))
         `(letrec ,(map (lambda (ids value)
                          `(,(lexical->sexp (car ids)) ,(code->sexp value)))
                        ids values)
            ,(code->sexp body))
         `(letrec-values ,(map (lambda (ids value)
                                 `(,(map lexical->sexp ids) ,(code->sexp value)))
                               ids values)
            ,(code->sexp body)))]
    [(s:fix ids procs body)
     `(fix ,(map (lambda (id proc)
                   `(,(id-name id) ,(code->sexp proc)))
                 ids procs)
        ,(code->sexp body))]
    [(s:define-values ids value)
     (if (= 1 (length ids))
         `(define ,(global->sexp (car ids))
            ,(code->sexp value))
         `(define-values ,(map global->sexp ids)
            ,(code->sexp value)))]
    [(s:set! var value)
     `(set! ,(code->sexp var) ,(code->sexp value))]
    [(s:lexical-init id value)
     `(init ,(lexical->sexp id) ,(code->sexp value))]
    [(s:global-init id value)
     `(init ,(global->sexp id) ,(code->sexp value))]
    [(s:program body)
     `(program ,@(map code->sexp body))]
    [(s:begin/var ids body)
     `(begin/var (ids ,@(map lexical->sexp ids))
                 ,@(map code->sexp body))]
    [(s:begin/const/var const-ids const-values ids body)
     `(begin/const/var (consts
                        ,@(map (lambda (id value)
                                 (list (lexical->sexp id) (code->sexp value)))
                               const-ids const-values))
                       (ids ,@(map lexical->sexp ids))
                       ,@(map code->sexp body))]
    [(s:program/var global-ids lexical-ids body)
     `(program/var (ids ,@(map global->sexp global-ids)
                        ,@(map lexical->sexp lexical-ids))
                 ,@(map code->sexp body))]
    [(s:program/const/var const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body)
     `(program/const/var (global-consts
                          ,@(map (lambda (id value)
                                   (list (global->sexp id)
                                         (code->sexp value)))
                                 const-global-ids const-global-values))
                         (lexical-consts
                          ,@(map (lambda (id value)
                                   (list (lexical->sexp id) (code->sexp value)))
                                 const-lexical-ids const-lexical-values))
                         (ids ,@(map global->sexp global-ids)
                              ,@(map lexical->sexp lexical-ids))
                         ,@(map code->sexp body))]
    [(s:loop ids values body)
     `(%loop ,(map (lambda (id value)
                     `(,(lexical->sexp id) ,(code->sexp value)))
                   ids values)
             ,(code->sexp body))]
    [(s:iterate args) `(%iterate ,@(map code->sexp args))]
    [(s:non-tail expr) `(non-tail ,(code->sexp expr))]
    [(s:tail expr) `(tail ,(code->sexp expr))]
    [(s:wcm key value expr) `(wcm ,(code->sexp key) ,(code->sexp value) ,(code->sexp expr))]
    [_ (error "compiler code->sexp: not matched" node)]))

(provide code->sexp)
