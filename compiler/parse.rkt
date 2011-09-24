#lang racket/base

(require racket/trace)

(require
 racket/match
 "deserialize.rkt"
 (prefix-in s: "syntax2.rkt")
 "utils.rkt")

(define (parse1 form)
  (parse form (make-immutable-hasheq '())))

(define (parse form lexical-env)
  (cond [(list? form)
         (if (null? form)
             (error 'parse "invalid null form")
             (parse-list form lexical-env))]
        [(or (real? form) (string? form) (char? form) (boolean? form))
         (s:literal form)]
        [(symbol? form)
         (s:lexical-ref (hash-ref lexical-env form))]
        [else (error 'parse "invalid form ~A" form)]))

(define (parse-list form lexical-env)
  (case (car form)
    [(begin)      (parse-begin form lexical-env)]
    [(fix)        (parse-fix form lexical-env)]
    [(if)         (parse-if form lexical-env)]
    [(lambda)     (parse-lambda form lexical-env)]
    [(let-values) (parse-let-values form lexical-env)]
    [else         (parse-app form lexical-env)]))

(define (parse-app form lexical-env)
  (match form
    [(list op args ...)
     (s:app (parse op lexical-env) (map (lambda (arg) (parse arg lexical-env)) args))]
    [_ (error 'parse "invalid app form ~A" form)]))

(define (parse-begin form lexical-env)
  (match form
    [(list 'begin form forms ...)
     (s:begin (map (lambda (form) (parse form lexical-env)) (cons form forms)))]
    [_ (error 'parse "invalid begin form ~A" form)]))

(define (parse-fix form lexical-env)
  (match form
    [(list 'fix (list bindings ...) body)
     (let ([ids (map (lambda (binding)
                       (match binding
                         [(list sym _)
                          (if (symbol? sym)
                              (new-lexical-id sym)
                              (error 'parse-fix "not a valid id ~A" sym))]))
                     bindings)])
       (let ([new-lexical-env (extended-lexical-env lexical-env (map car bindings) ids)])
         (let ([procs (map (lambda (binding) (parse-lambda (cadr binding) new-lexical-env)) bindings)])
           (s:fix ids procs (parse body new-lexical-env)))))]
    [_ (error 'parse "invalid fix form ~A" form)]))

(define (parse-if form lexical-env)
  (match form
    [(list 'if test then else)
     (s:if (parse test lexical-env) (parse then lexical-env) (parse else lexical-env))]
    [_ (error 'parse "invalid if form ~A" form)]))

(define (parse-lambda form lexical-env)
  (match form
    [(list 'lambda formals body)
     (match formals
       [(and sym (? symbol?))
        (let ([id (new-lexical-id sym)])
          (s:lambda '() id (parse body (extended-lexical-env lexical-env (list sym) (list id)))))]
       [(list syms ...)
        (unless (andmap symbol? syms)
          (error 'parse-lambda "invalid formals ~A" formals))
        (let ([ids (map new-lexical-id syms)])
          (s:lambda ids #f (parse body (extended-lexical-env lexical-env syms ids))))]
       [(list-rest syms ... rest-sym)
        (unless (and (andmap symbol? syms) (symbol? rest-sym))
          (error 'parse-lambda "invalid formals ~A" formals))
        (let ([ids (map new-lexical-id syms)]
              [rest-id (new-lexical-id rest-sym)])
          (s:lambda ids rest-id (parse body (extended-lexical-env (cons rest-sym syms) (cons rest-id ids)))))]
       [_ (error 'parse-lambda "invalid formals in ~A" form)])]
    [_ (error 'parse "invalid lambda form ~A" form)]))

(define (parse-let-values form lexical-env)
  (match form
    [(list 'let-values (list bindings ...) body)
     (let ([idss (map (lambda (binding)
                        (match binding
                          [(list syms _)
                           (if (and (list? syms)
                                    (andmap symbol? syms))
                               (map new-lexical-id syms)
                               (error 'parse-let-values "not a valid ids list ~A" syms))]))
                      bindings)])
       (let ([new-lexical-env (extended-lexical-env lexical-env (apply append (map car bindings)) (apply append idss))])
         (let ([procs (map (lambda (binding) (parse (cadr binding) new-lexical-env)) bindings)])
           (s:let-values idss procs (parse body new-lexical-env)))))]
    [_ (error 'parse "invalid fix form ~A" form)]))

(define (extended-lexical-env lexical-env syms ids)
  (foldl (lambda (sym id env) (hash-set env sym id))
         lexical-env syms ids))

(define (unparse stx)
  (code->sexp stx))

(provide (rename-out [parse1 parse])
         unparse)