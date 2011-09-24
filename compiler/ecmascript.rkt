#lang scheme/base

(require scheme/base
         scheme/list
         scheme/match
         scheme/contract

         "../externals/javascript/ast.ss"
         (only-in "../externals/javascript/parse.ss"
                  make-region
                  make-position)

         "syntax.rkt")

;;; Declarations

(define (fn-decl #:loc [loc #f] name params . body)
  (make-FunctionDeclaration (loc->region loc)
                            (->identifier name)
                            (map ->identifier params)
                            (splice-blocks (->statement-list body))))

(define (var-decl #:loc [loc #f] . assoc-list)
  (make-VariableDeclaration (loc->region loc)
                            (map ->variable-initializer assoc-list)))

(define (let-decl #:loc [loc #f] . assoc-list)
  (make-LetDeclaration (loc->region loc)
                       (map ->variable-initializer assoc-list)))

(define (var-init #:loc [loc #f] id expr)
  (make-VariableInitializer loc
                            (->identifier id)
                            (->expression expr)))

;;; Expressions

(define (_string #:loc [loc #f] string)
  (make-StringLiteral (loc->region loc) string))

;; TODO: regexp

(define (number #:loc [loc #f] real)
  (make-NumericLiteral (loc->region loc) real))

(define (boolean #:loc [loc #f] boolean)
  (make-BooleanLiteral (loc->region loc) boolean))

(define (_null #:loc [loc #f])
  (make-NullLiteral (loc->region loc)))

(define (array #:loc [loc #f] . elems)
  (make-ArrayLiteral (loc->region loc)
                     (map (lambda (e) (paren-if (->expression e) (ListExpression? e)))
                          elems)))

(define (object #:loc [loc #f] . assocs)
  (make-ObjectLiteral (loc->region loc)
                      (map (lambda (pair)
                             (match pair
                               [(cons name value)
                                (let ([e (->expression name)])
                                  (cons (if (Property? e)
                                            e
                                            (error 'object "invalid property ~S" name))
                                        (paren-if (->expression value) (ListExpression? value))))]))
                           assocs)))

(define (_this #:loc [loc #f])
  (make-ThisReference (loc->region loc)))

(define (var-ref #:loc [loc #f] id)
  (make-VarReference (loc->region loc) (->identifier id)))

(define (_field #:loc [loc #f] container key)
  (make-BracketReference (loc->region loc) (->expression container) (->expression key)))

(define (_new #:loc [loc #f] constructor . args)
  (make-NewExpression (loc->region loc)
                      (->expression constructor)
                      (map (lambda (e)
                             (paren-if (->expression e) (ListExpression? e)))
                           args)))

(define (x++ #:loc [loc #f] expr)
  (make-PostfixExpression (loc->region loc) (->expression expr) '++))

(define (x-- #:loc [loc #f] expr)
  (make-PostfixExpression (loc->region loc) (->expression expr) '--))

(define (delete #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) 'delete (->expression expr)))

(define (_void #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) 'void (->expression expr)))

(define (typeof #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) 'typeof (->expression expr)))

(define (++x #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) '++ (->expression expr)))

(define (--x #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) '-- (->expression expr)))

(define (+x #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) '+ (->expression expr)))

(define (-x #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) '- (->expression expr)))

(define (~x #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) '~ (->expression expr)))

(define (!x #:loc [loc #f] expr)
  (make-PrefixExpression (loc->region loc) '! (->expression expr)))

(define (_* #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '* (->expression expr2)))

(define (_/ #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '/ (->expression expr2)))

(define (_% #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '% (->expression expr2)))

(define (_+ #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '+ (->expression expr2)))

(define (_- #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '- (->expression expr2)))

(define (_<< #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '<< (->expression expr2)))

(define (_>> #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '>> (->expression expr2)))

(define (_>>> #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '>>> (->expression expr2)))

(define (_< #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '< (->expression expr2)))

(define (_> #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '> (->expression expr2)))

(define (_<= #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '<= (->expression expr2)))

(define (_>= #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '>= (->expression expr2)))

(define (instance-of #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) 'instanceof (->expression expr2)))

(define (in #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) 'in (->expression expr2)))

(define (_== #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '== (->expression expr2)))

(define (_!= #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '!= (->expression expr2)))

(define (_=== #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '=== (->expression expr2)))

(define (_!== #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '!== (->expression expr2)))

(define (_& #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '& (->expression expr2)))

(define (_^ #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '^ (->expression expr2)))

(define (_\| #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '\| (->expression expr2)))

(define (_&& #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '&& (->expression expr2)))

(define (_\|\| #:loc [loc #f] expr1 expr2)
  (make-InfixExpression (loc->region loc) (->expression expr1) '\|\| (->expression expr2)))

(define (if-expr #:loc [loc #f] test then [else (void)])
  (make-ConditionalExpression (loc->region loc)
                              (->expression test)
                              (->expression then)
                              (->expression else)))

(define (_= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '= (->expression expr2)))

(define (_*= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '*= (->expression expr2)))

(define (_/= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '/= (->expression expr2)))

(define (_%= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '%= (->expression expr2)))

(define (_+= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '+= (->expression expr2)))

(define (_-= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '-= (->expression expr2)))

(define (_<<= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '<<= (->expression expr2)))

(define (_>>= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '>>= (->expression expr2)))

(define (_>>>= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '>>>= (->expression expr2)))

(define (_&= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '&= (->expression expr2)))

(define (_^= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '^= (->expression expr2)))

(define (_\|= #:loc [loc #f] expr1 expr2)
  (make-AssignmentExpression (loc->region loc) (->expression expr1) '\|= (->expression expr2)))

(define (fn #:loc [loc #f] params . body)
  (make-FunctionExpression (loc->region loc)
                           #f
                           (map ->identifier params)
                           (splice-blocks (->statement-list body))))

(define (named-fn #:loc [loc #f] name params . body)
  (make-FunctionExpression (loc->region loc)
                           (->identifier name)
                           (map ->identifier params)
                           (splice-blocks (->statement-list body))))

(define (let-expr #:loc [loc #f] assoc-list expr)
  (make-LetExpression (loc->region loc)
                      (map ->variable-initializer assoc-list)
                      (->expression expr)))

(define (call #:loc [loc #f] proc . args)
  (make-CallExpression (loc->region loc)
                       (->expression proc) ;; TODO
                       (map (lambda (e)
                              (paren-if (->expression e) (ListExpression? e)))
                            args)))

(define (comma #:loc [loc #f] . exprs)
  (make-ListExpression (loc->region loc) (map ->expression exprs)))

;;; Statements

(define (expr #:loc [loc #f] expr)
  (make-ExpressionStatement (loc->region loc) (->expression expr)))

(define (block #:loc [loc #f] . statements)
  (let ([body (splice-blocks (->statement-list statements))])
    (cond [(null? body)
           (_empty #:loc loc)]
          [(null? (cdr body))
           (car body)]
          [else
           (make-BlockStatement (loc->region loc) body)])))

(define (splice-blocks stmts)
  (append-map (lambda (s)
                (if (BlockStatement? s)
                    (BlockStatement-statements s)
                    (list s)))
              stmts))

(define (_empty #:loc [loc #f])
  (make-EmptyStatement (loc->region loc)))

(define (if-stmt #:loc [loc #f] test then [else (_empty)])
  (make-IfStatement (loc->region loc) (->expression test) (->statement then)
                    (let ([s (->statement else)])
                      (and (not (EmptyStatement? s))
                           s))))

(define (do-while #:loc [loc #f] test . body)
  (make-DoWhileStatement (loc->region loc) (block (->statement-list body)) (->expression test)))

(define (while #:loc [loc #f] test . body)
  (make-WhileStatement (loc->region loc) (->expression test)
                       (block (->statement-list body))))

(define (_for #:loc [loc #f] init test incr . body)
  (make-ForStatement (loc->region loc)
                     init
                     (->expression test)
                     (->expression incr)
                     (block (->statement-list body))))

(define (for-in #:loc [loc #f] lhs container . body)
  (make-ForInStatement (loc->region loc) lhs (->expression container)
                       (block (->statement-list body))))

(define (continue #:loc [loc #f] [label #f])
  (make-ContinueStatement (loc->region loc) (and label (->identifier label))))

(define (break #:loc [loc #f] [label #f])
  (make-BreakStatement (loc->region loc) (and label (->identifier label))))

(define (return #:loc [loc #f] value)
  (make-ReturnStatement (loc->region loc) (->expression value)))

(define (let-stmt #:loc [loc #f] assoc-list . body)
  (make-LetStatement (loc->region loc)
                     (map ->variable-initializer assoc-list)
                     (block (->statement-list body))))

(define (with #:loc [loc #f] expr . body)
  (make-WithStatement (loc->region loc)
                      (->expression expr)
                      (block (->statement-list body))))

(define (switch #:loc [loc #f] expr . cases)
  (make-SwitchStatement (loc->region loc)
                        (->expression expr)
                        cases))

(define (_case #:loc [loc #f] expr . body)
  (make-CaseClause (loc->region loc)
                   (->expression expr)
                   (splice-blocks (->statement-list body))))

(define (label #:loc [loc #f] label stmt)
  (make-LabelledStatement (loc->region loc) (->identifier label) (->statement stmt)))

(define (throw #:loc [loc #f] value)
  (make-ThrowStatement (loc->region loc) (->expression value)))

(define (try #:loc [loc #f] body . rest)
  (define fn
    (case-lambda
      [(id catch finally)
       (list (list (make-CatchClause #f
                                     (->identifier id)
                                     (->block catch)))
             (->block finally))]
      [(id catch)
       (list (list (make-CatchClause #f
                                     (->identifier id)
                                     (->block catch)))
             #f)]
      [(finally)
       (list #f (->block finally))]))
  (apply make-TryStatement (loc->region loc) (->block body) (apply fn rest)))

;;;; Tools

(define (loc->region loc)
  (and loc
       (make-region (loc-source loc)
                    (make-position (loc-position loc)
                                   (loc-line loc)
                                   (loc-column loc))
                    (make-position (+ (or (loc-position loc) 0)
                                      (loc-span loc))
                                   (loc-line loc)
                                   (+ (or (loc-column loc) 0)
                                      (loc-span loc))))))


(define (->identifier a)
  (cond [(Identifier? a)
         a]
        [(symbol? a)
         (make-Identifier #f a)]
        [else
         (error '->identifier "got ~S" a)]))

(define  (->variable-initializer a)
  (if (VariableInitializer? a)
      a
      (match a
        [(cons id init)
         (make-VariableInitializer #f
                                   (->identifier id)
                                   (paren-if (->expression init) (ListExpression? init)))]
        [_
         (make-VariableInitializer #f (->identifier a) #f)])))

(define (paren-if expr cond)
  (if cond
      (make-ParenExpression #f expr)
      expr))

(define (->expression obj)
  (cond [(Expression? obj)
         obj]
        [(void? obj)    #f]
        [(string? obj)  (make-StringLiteral #f obj)]
        [(real? obj)    (make-NumericLiteral #f obj)]
        [(boolean? obj) (make-BooleanLiteral #f obj)]
        [(vector? obj)  (make-ArrayLiteral #f (map ->expression (vector->list obj)))]
        [(null? obj)    (make-NullLiteral #f)]
        [(symbol? obj)  (make-VarReference #f (->identifier obj))]))

(define (->expression? x)
  (or (Expression? x)
      (void? x)
      (string? x)
      (real? x)
      (boolean? x)
      (vector? x)
      (null? x)
      (symbol? x)))

(define (->statement obj)
  (cond [(or (Declaration? obj) (Statement? obj))
         obj]
        [(list? obj) (apply block #f (->statement-list obj))]
        [else        (make-ExpressionStatement #f (->expression obj))]))

(define (->block obj)
  (let ([s (->statement obj)])
    (if (BlockStatement? s)
        s
        (make-BlockStatement #f (list s)))))

(define (->statement-list l)
  (foldr (lambda (s l)
           (let ([s (->statement s)])
             (if (or (EmptyStatement? s)
                     (and (ExpressionStatement? s)
                          (constant-expr? (ExpressionStatement-expression s))))
                 l
                 (cons s l))))
         '()
         l))

(define (constant-expr? e)
  (or (StringLiteral? e)
      (RegexpLiteral? e)
      (NumericLiteral? e)
      (BooleanLiteral? e)
      (NullLiteral? e)
      (ThisReference? e)
      (VarReference? e)))

(provide ->expression)

(provide fn-decl var-decl let-decl var-init
         (rename-out (_string string)
                     (number number)
                     (boolean boolean))
         (rename-out (_null null))
         array
         object
         (rename-out (_this this))
         var-ref
         (rename-out (_field field)
                     (_new new))
         x++ x-- delete
         (rename-out (_void void))
         typeof
         ++x --x +x -x ~x !x
         (rename-out (_* *)
                     (_/ /)
                     (_% %)
                     (_+ +)
                     (_- -)
                     (_<< <<)
                     (_>> >>)
                     (_>>> >>>)
                     (_< <)
                     (_> >)
                     (_<= <=)
                     (_>= >=))
         instance-of in
         (rename-out (_== ==)
                     (_!= !=)
                     (_=== ===)
                     (_!== !==)
                     (_& &)
                     (_^ ^)
                     (_\| \|)
                     (_&& &&)
                     (_\|\| \|\|))
         (rename-out (if-expr if-e))

         (rename-out (_= =)
                     (_*= *=)
                     (_/= /=)
                     (_%= %=)
                     (_+= +=)
                     (_-= -=)
                     (_<<= <<=)
                     (_>>= >>=)
                     (_>>>= >>>=)
                     (_&= &=)
                     (_^= ^=)
                     (_\|= \|=))
         fn named-fn comma
         expr
         block
         (rename-out (_empty empty))
         (rename-out (if-stmt if))
         do-while while
         (rename-out (_for for))
         for-in continue break return let-stmt with
         switch
         (rename-out (_case case))
         label throw try)

(provide/contract
 [let-expr (-> any/c any/c #:loc (or/c #f loc?) LetExpression?)]
 [call (->* (->expression?) (#:loc (or/c #f loc?)) #:rest (listof ->expression?) CallExpression?)])
