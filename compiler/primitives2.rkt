#lang at-exp scheme/base

(require
 scheme/base
 (for-syntax scheme/base)
 scheme/match
 "syntax.rkt"
 (for-syntax "stx.rkt")
 (prefix-in c: "ecmascript.rkt")
 "store.rkt"
 "snippets.rkt"
 "backend.rkt"
 "primitives.rkt")

;;;; Scheme primitives

;;; 5 Equivalence predicates

(define-primitive %%eq?
  (lambda (loc a b)
    @js[#:loc loc]{@a === @b})
  #:referencial-transparent!)

(provide %%eq?)

;;; 6 Procedure predicate

(define-primitive %%procedure?
  (lambda (loc obj)
    @js[#:loc loc]{typeof @obj === "function"})
  #:referencial-transparent!)

(provide %%procedure?)

;;; 7.4 Arithmetic: Numerical operations

(define-primitive %%number?
  (lambda (loc obj)
    @js[#:loc loc]{typeof @obj === "number"})
  #:referencial-transparent!)

(define-primitive %%=
  (lambda (loc a b)
    @js[#:loc loc]{ @a == @b})
  #:referencial-transparent!)

(define-primitive %%<
  (lambda (loc a b) (c:< a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%<=
  (lambda (loc a b) (c:<= a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%+
  (lambda (loc a b) (c:+ a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%*
  (lambda (loc a b) (c:* a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%-
  (lambda (loc a b) (c:- a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%/
  (lambda (loc a b) (c:/ a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%mod
  (lambda (loc a b) (c:% a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%floor
  (lambda (loc x) @js[#:loc loc]{Math.floor(@x)})
  #:referencial-transparent!)

(define-primitive %%ceiling
  (lambda (loc x) @js[#:loc loc]{Math.ceil(@x)})
  #:referencial-transparent!)

(define-primitive %%round
  (lambda (loc x) @js[#:loc loc]{Math.round(@x)})
  #:referencial-transparent!)

(define-primitive %%exp
  (lambda (loc x) @js[#:loc loc]{Math.exp(@x)})
  #:referencial-transparent!)

(define-primitive %%log
  (lambda (loc x) @js[#:loc loc]{Math.log(@x)})
  #:referencial-transparent!)

(define-primitive %%log2
  (lambda (loc x) @js[#:loc loc]{Math.LOG2E * Math.log(@x)})
  #:referencial-transparent!)

(define-primitive %%sin
  (lambda (loc x) @js[#:loc loc]{Math.sin(@x)})
  #:referencial-transparent!)

(define-primitive %%cos
  (lambda (loc x) @js[#:loc loc]{Math.cos(@x)})
  #:referencial-transparent!)

(define-primitive %%tan
  (lambda (loc x) @js[#:loc loc]{Math.tan(@x)})
  #:referencial-transparent!)

(define-primitive %%asin
  (lambda (loc x) @js[#:loc loc]{Math.asin(@x)})
  #:referencial-transparent!)

(define-primitive %%acos
  (lambda (loc x) @js[#:loc loc]{Math.acos(@x)})
  #:referencial-transparent!)

(define-primitive %%atan
  (lambda (loc x) @js[#:loc loc]{Math.atan(@x)})
  #:referencial-transparent!)

(define-primitive %%atan2
  (lambda (loc x y) @js[#:loc loc]{Math.atan2(@x, @y)})
  #:referencial-transparent!)

(define-primitive %%sqrt
  (lambda (loc x) @js[#:loc loc]{Math.sqrt(@x)})
  #:referencial-transparent!)

(define-primitive %%expt
  (lambda (loc x y) @js[#:loc loc]{Math.pow(@x, @y)})
  #:referencial-transparent!)

(define-primitive %%number->string
  (lambda (loc x y) @js[#:loc loc]{(@x).toString(@y)}))

(provide %%floor
         %%ceiling
         %%round
         %%exp
         %%log
         %%log2
         %%sin
         %%cos
         %%tan
         %%asin
         %%acos
         %%atan
         %%atan2
         %%sqrt
         %%expt
         %%number->string)

(define-primitive %%bitwise-ior
  (lambda (loc a b) (c:\| a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%bitwise-and
  (lambda (loc a b) (c:& a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%bitwise-xor
  (lambda (loc a b) (c:^ a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%bitwise-not
  (lambda (loc b) (c:~x b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%shift-left
  (lambda (loc n m) (c:<< n m #:loc loc))
  #:referencial-transparent!)

(define-primitive %%shift-right
  (lambda (loc n m) (c:>> n m #:loc loc))
  #:referencial-transparent!)

(provide %%number?
         %%= %%< %%<=
         %%+ %%* %%- %%/
         %%mod
         %%bitwise-ior
         %%bitwise-and
         %%bitwise-xor
         %%bitwise-not
         %%shift-left
         %%shift-right)

;;; 8 Booleans

(define-primitive %%boolean?
  (lambda (loc obj)
    (c:=== (c:typeof obj) (c:string "boolean") #:loc loc))
  #:referencial-transparent!)

(define-primitive %%boolean=?
  (lambda (loc a b)
    @js[#:loc loc]{@a === @b})
  #:referencial-transparent!)

(define-primitive %%not
  (lambda (loc b) (c:=== b #f #:loc loc))
  #:referencial-transparent!)

(provide %%boolean? %%boolean=? %%not)

;;; 9 Pairs and lists

(define-primitive %%pair?
  (lambda (loc obj)
    (c:call (c:field (c:\|\| obj (c:object)) "hasOwnProperty") car-property-name
            #:loc loc))
  #:referencial-transparent!)

(define-primitive %%cons
  (lambda (loc a b)
    (c:object (cons (c:string car-property-name) a)
              (cons (c:string cdr-property-name) b)
              #:loc loc))
  #:referencial-transparent!)

(define-primitive %%car
  (lambda (loc pair)
    (c:field pair car-property-name #:loc loc))
  #:referencial-transparent!)

(define-primitive %%cdr
  (lambda (loc pair)
    (c:field pair cdr-property-name #:loc loc))
  #:referencial-transparent!)

(provide %%pair? %%cons %%car %%cdr)

;;; 10 Symbols

(define-primitive %%symbol?
  (lambda (loc objp)
    (let ([obj 'za])
      @js[#:loc loc]{
        (function (@obj) {
             return (typeof @obj) === "string" && @|obj|.length > 0 && @|obj|.charCodeAt(0) === @#xEBAC;
         })(@objp)}))
  #:referencial-transparent!)

(define-primitive %%symbol=?
  (lambda (loc a b)
    @js[#:loc loc]{@a === @b})
  #:referencial-transparent!)

(define-primitive %%symbol->string
  (lambda (loc sym) (c:call (c:field sym "slice") 1))
  #:referencial-transparent!)

(define-primitive %%string->symbol
  (lambda (loc str) (c:+ "\uEBAC" str))
  #:referencial-transparent!)

(provide %%symbol? %%symbol=? %%symbol->string %%string->symbol)

;;; 11 Characters

(define-primitive %%char?
  (lambda (loc obj)
    (c:call (c:field (c:\|\| obj (c:object)) "hasOwnProperty") char->integer-property-name
            #:loc loc))
  #:referencial-transparent!)

(define-primitive %%char->integer
  (lambda (loc a) (c:field a (c:string char->integer-property-name) #:loc loc))
  #:referencial-transparent!)

(define-primitive %%integer->char
  (lambda (loc a) (c:object (cons (c:string char->integer-property-name) a) #:loc loc))
  #:referencial-transparent!)

;; The following functions are not correct.

(define-primitive %%char=?
  (lambda (loc a b)
    (c:=== (c:field a (c:string char->integer-property-name))
           (c:field b (c:string char->integer-property-name))
           #:loc loc))
  #:referencial-transparent!)

(define-primitive %%char<?
  (lambda (loc a b)
    (c:< (c:field a (c:string char->integer-property-name))
         (c:field b (c:string char->integer-property-name))
         #:loc loc))
  #:referencial-transparent!)

(define-primitive %%char<=?
  (lambda (loc a b)
    (c:<= (c:field a (c:string char->integer-property-name))
          (c:field b (c:string char->integer-property-name))
          #:loc loc))
  #:referencial-transparent!)

(provide %%char? %%char->integer %%integer->char
         %%char=? %%char<? %%char<=?)

;;; 12 String

(define-primitive %%string?
  (lambda (loc objp)
    (let ([obj 'za])
      @js[#:loc loc]{
        (function (@obj) {
             return (typeof @obj) === "string" && !(@|obj|.length > 0 && @|obj|.charCodeAt(0) === @#xEBAC);
         })(@objp)}))
  #:referencial-transparent!)

(define-primitive %%make-string
  (lambda (loc kp charp)
    (let ([k 'za] [char 'zb] [a 'zc] [s 'zd] [i 'ze])
      @js[#:loc loc]{
        (function (@k, @char) {
             var @a = new Array(@k);
             var @s = String.fromCharCode(@|char|["@char->integer-property-name"]);
             for (var @i = 0;
                  @i < @k;
                  @|i|++)
                 @|a|[@|i|] = @s;
             return @|a|.join("");
         })(@kp, @charp)}))
  #:referencial-transparent!)

(define-primitive %%string-length
  (lambda (loc str)
    @js[#:loc loc]{@|str|.length})
  #:referencial-transparent!)

(define-primitive %%string-ref
  (lambda (loc str i)
    (c:object (cons (c:string char->integer-property-name)
                    (c:call (c:field str (c:string "charCodeAt")) i)) #:loc loc))
  #:referencial-transparent!)

(define-primitive %%string=?
  (lambda (loc a b)
    @js[#:loc loc]{ @a == @b})
  #:referencial-transparent!)

(define-primitive %%string<?
  (lambda (loc a b)
    @js[#:loc loc]{ @a < @b})
  #:referencial-transparent!)

(define-primitive %%string<=?
  (lambda (loc a b)
    @js[#:loc loc]{ @a <= @b})
  #:referencial-transparent!)

(define-primitive %%substring
  (case-lambda
    [(loc str start)     (c:call #:loc loc (c:field str "slice") start)]
    [(loc str start end) (c:call #:loc loc (c:field str "slice") start end)]))

(define-primitive %%string-append
  (lambda (loc a b) (c:+ a b #:loc loc))
  #:referencial-transparent!)

(define-primitive %%list->string
  (lambda (loc lstp)
    (let ([lst 'za] [a 'zb] [p 'zc])
      @js[#:loc loc]{
        (function (@lst) {
             var @a = [];
             for (var @p = @lst;
                  @p !== null;
                  @p = @|p|["@cdr-property-name"]) {
                 @|a|.push(@|p|["@car-property-name"]["@char->integer-property-name"]);
             }
             return String.fromCharCode.apply(String, @a);
         })(@lstp)}))
  #:referencial-transparent!)

(provide %%string?
         %%make-string
         %%string-length
         %%string-ref
         %%string=?
         %%string<?
         %%string<=?
         %%substring
         %%string-append
         %%list->string)

;;; 13 Vectors

(define-primitive %%vector?
  (lambda (loc obj)
    @js[#:loc loc]{@obj instanceof Array})
  #:referencial-transparent!)

(define-primitive %%make-vector
  (lambda (loc k)
    @js[#:loc loc]{new Array(@k)})
  #:side-effect-free!)

(define-primitive %%vector-length
  (lambda (loc vec)
    @js[#:loc loc]{@|vec|.length})
  #:side-effect-free!)

(define-primitive %%vector-ref
  (lambda (loc vec i)
    @js[#:loc loc]{@|vec|[@i]})
  #:side-effect-free!)

(define-primitive %%vector-set!
  (lambda (loc vec i obj)
    @js[#:loc loc]{@|vec|[@i] = @obj}))

(provide %%vector?
         %%make-vector
         %%vector-length
         %%vector-ref
         %%vector-set!)

;;; 14 Errors and violations

(define-primitive with-exception-handler-procedure
  (lambda (loc continuation-index-var
               continuation-state-var
               store-continuation-exception-var
               switch-continuation-exception-var
               tail-object-var tail-trampoline-var tail-trampoline-restart-var)
    (let ([marks 'za] [proc 'zb] [thunk 'zc] [exc 'zd] [aux 'ze])
      (c:fn (list marks proc thunk)
            (c:var-decl (cons 'zp 0)
                        (cons 'zr (c:field tail-object-var tail-object-calls-property-name))
                        'zs exc)
            (recover-continuation-code continuation-index-var
                                       continuation-state-var
                                       (list 'zp 'zr 'zs marks proc thunk exc))
            (c:try (c:switch 'zp
                             (c:case 0
                                     (c:try
                                      (c:return (c:call thunk marks))
                                      aux
                                      (c:if (c:\|\| (c:instance-of aux store-continuation-exception-var)
                                                    (c:instance-of aux switch-continuation-exception-var))
                                            (c:throw aux)
                                            (c:= exc aux))))
                             (c:case 1
                                     (c:= 'zp  1)
                                     (tail-call-trampoline proc
                                                           (list marks exc)
                                                           tail-object-var
                                                           tail-trampoline-var
                                                           tail-trampoline-restart-var
                                                           'zr 'zs)))
                   'zq
                   (c:block
                    (create-stack-code continuation-state-var
                                       store-continuation-exception-var
                                       'zq
                                       (c:array 'zp 'zr 'zs marks proc thunk exc))

                    (c:throw 'zq))))))
  #:fixed-denotation procedure)

(provide with-exception-handler-procedure)

;;; 15 Control features

(define-primitive apply-procedure
  (lambda (loc continuation-index-var
               continuation-state-var
               store-continuation-exception-var
               tail-object-var tail-trampoline-var tail-trampoline-restart-var)
    (let ([marks 'za] [proc 'zb] [len 'zc] [array 'zd] [last 'ze])
      (c:fn (list marks proc)
            (c:var-decl (cons 'zp 0)
                        (cons 'zr (c:field tail-object-var tail-object-calls-property-name))
                        'zs len array last)
            (recover-continuation-code continuation-index-var
                                       continuation-state-var
                                       (list 'zp 'zr 'zs marks proc array))
            (c:try
             (c:switch
              'zp
              (c:case 0
                      (c:= len (c:field 'arguments "length"))
                      (c:if (c:< len 3)
                            (c:throw (c:new 'Error "apply needs two or more arguments")))
                      (c:= array (c:call (c:field (c:field (c:field 'Array "prototype")
                                                          "slice")
                                                 "call")
                                        'arguments 1 (c:- len 1)))
                      (c:= (c:field array 0) marks)
                      (c:= last (c:field 'arguments (c:- len 1)))
                      (c:while (c:call (c:field (c:\|\| last (c:object)) "hasOwnProperty") car-property-name)
                               (c:call (c:field array "push")
                                       (c:field last car-property-name))
                               (c:= last (c:field last cdr-property-name)))
                      (c:if (c:!== last '())
                            (c:throw (c:new 'Error "last argument must be a list"))))
              (c:case 1
                      (c:= 'zp 1)
                      (tail-call-trampoline proc
                                            (list array)
                                            #:apply? #t
                                            tail-object-var
                                            tail-trampoline-var
                                            tail-trampoline-restart-var
                                            'zr 'zs)))
             'zq
             (c:block
              (create-stack-code continuation-state-var
                                 store-continuation-exception-var
                                 'zq
                                 (c:array 'zp 'zr 'zs marks proc array))
              (c:throw 'zq))))))
  #:fixed-denotation procedure)

;; (define-primitive suspend-procedure
;;   (lambda (loc continuation-exception-constructor continuation-index)
;;     (c:fn '(h)
;;                (c:if continuation-index
;;                            (c:= continuation-index (c:boolean #f))
;;                            (c:throw (c:new continuation-exception-constructor
;;                                            (list (c:var-ref 'h))))))))

(define-primitive call/cc-procedure
  (lambda (loc continuation-index-var
               continuation-state-var
               store-continuation-exception-var
               switch-continuation-exception-var
               current-toplevel-var
               tail-object-var tail-trampoline-var tail-trampoline-restart-var)
    (let ([proc 'za] [kont 'zb] [value 'zc] [exception 'zd] [stack 'ze] [toplevel 'zf]
          [len 'zg] [callee-in-stack? 'zh] [marks 'zi])
      (let ([k (c:fn (list marks value)
                     (c:= continuation-state-var stack)
                     (c:= continuation-index-var (c:field stack "length"))
                     (c:= (c:field (c:field stack 0) 6)
                          (c:if-e (c:=== (c:field 'arguments "length") 2)
                                  value
                                  (c:object (cons (c:string values-property-name) (c:call (c:field (c:field (c:field 'Array "prototype")
                                                                                                            "slice")
                                                                                                   "call")
                                                                                          'arguments 1)))))
                     (c:= current-toplevel-var toplevel)
                     (c:throw (c:new switch-continuation-exception-var #t)))])
        (c:fn (list marks proc)
              (c:var-decl (cons 'zp 0)
                          (cons 'zr (c:field tail-object-var tail-object-calls-property-name))
                          'zs kont value)
              (recover-continuation-code
               continuation-index-var
               continuation-state-var
               (list 'zp 'zr 'zs marks proc kont value)
               #:if-not-recovering
               (c:block
                (c:= continuation-state-var (c:array (c:array 'zp 'zr 'zs marks proc kont value)))
                (c:throw (c:new store-continuation-exception-var))))
              (c:try (c:switch 'zp
                               (c:case 0
                                       (c:var-decl (cons stack continuation-state-var)
                                                   (cons toplevel current-toplevel-var))
                                       (c:= (c:field (c:field stack 0) 0) 2)
                                       (c:= kont k)
                                       (c:= (c:field kont continuation-marks-property-name)
                                            marks))

                               (c:case 1
                                       (c:= 'zp 1)
                                       (tail-call-trampoline proc
                                                             (list marks kont)
                                                             tail-object-var
                                                             tail-trampoline-var
                                                             tail-trampoline-restart-var
                                                             'zr 'zs))
                               (c:case 2
                                       (c:= continuation-index-var -1)
                                       (c:return value)))
                     'zq
                     (c:block
                      (create-stack-code continuation-state-var
                                         store-continuation-exception-var
                                         'zq
                                         (c:array 'zp 'zr 'zs marks proc kont value))
                      (c:throw 'zq)))))))
  #:fixed-denotation procedure)

;; catch (zq) {
;;     if (!(zq instanceof c))
;;         if (zq instanceof b) {
;;             if (typeof zq["c"][zq["c"]["length"] - 1] !== "function") {
;;                 zq.c.concat([[1, za, zb, zc]], ze.slice(1));
;;             } else {
;;                 if (ze.length > 1 && typeof ze[1] === "function")
;;                     zq.c.concat(ze.slice(2));
;;                 else
;;                     zq.c.concat(ze);
;;             }
;;             zq.c.s = zq.c.slice();
;;             zq.m = zb.p0 ;
;;             throw new c(zq.c, true);
;;         }
;;     throw zq;
;; }

(define-primitive %%values
  (lambda (loc a)
    (c:object (cons (c:string values-property-name) a)
              #:loc loc))
  #:referencial-transparent!)

(define-primitive %%from-values
  (lambda (loc vals i)
    (c:field (c:field vals values-property-name) i
             #:loc loc))
  #:referencial-transparent!)

(define-primitive call-with-values-procedure
  (lambda (loc continuation-index-var
               continuation-state-var
               store-continuation-exception-var
               tail-object-var tail-trampoline-var tail-trampoline-restart-var)
    (let ([marks 'za] [producer 'zb] [consumer 'zc] [value 'zd] [aux 'ze])
      (c:fn (list marks producer consumer)
            (c:var-decl (cons 'zp 0)
                        (cons 'zr (c:field tail-object-var tail-object-calls-property-name))
                        'zs value)
            (recover-continuation-code continuation-index-var
                                       continuation-state-var
                                       (list 'zp 'zr 'zs marks producer consumer value))
            (c:try (c:switch 'zp
                             (c:case 0
                                     (c:= value (c:call producer marks)))
                             (c:case 1
                                     (c:= 'zp  1)
                                     (c:if (c:call (c:field (c:\|\| value (c:object)) "hasOwnProperty") values-property-name)
                                           (c:block
                                            (c:var-decl (cons aux (c:field value
                                                                           values-property-name)))
                                            (c:= aux (c:call (c:field aux "slice")))
                                            (c:call (c:field aux "unshift") marks)
                                            (tail-call-trampoline consumer
                                                                  (list aux)
                                                                  #:apply? #t
                                                                  tail-object-var
                                                                  tail-trampoline-var
                                                                  tail-trampoline-restart-var
                                                                  'zr 'zs))
                                           (tail-call-trampoline consumer
                                                                 (list marks value)
                                                                 tail-object-var
                                                                 tail-trampoline-var
                                                                 tail-trampoline-restart-var
                                                                 'zr 'zs))))
                   'zq
                   (c:block
                    (create-stack-code continuation-state-var
                                       store-continuation-exception-var
                                       'zq
                                       (c:array 'zp 'zr 'zs marks producer consumer value))
                    (c:throw 'zq))))))
  #:fixed-denotation procedure)

    ;; (let ([k (c:number 42)]) ;; k is the current-continuation
    ;;   (c:call producer
    ;;           (c:fn '()
    ;;                 (c:call (c:field (c:field (c:field 'Array "prototype")
    ;;                                           "unshift")
    ;;                                  "call")
    ;;                         'arguments
    ;;                         k)
    ;;                 (c:return (c:call (c:field consumer "apply") (c:null) 'arguments)))
    ;;           #:loc loc))

(define-primitive abort-current-continuation-procedure
  (lambda (loc switch-continuation-exception-constructor)
    (c:fn '()
          (c:throw (c:new switch-continuation-exception-constructor #f))))
  #:fixed-denotation procedure)

(provide apply-procedure
         abort-current-continuation-procedure
         call/cc-procedure
         %%values
         %%from-values
         call-with-values-procedure)

;;;

(define-primitive %%box
  (lambda (loc a)
    (c:object (cons (c:string box-value-property-name) a) #:loc loc))
  #:side-effect-free!)

(define-primitive %%unbox
  (lambda (loc a)
    (c:field a (c:string box-value-property-name) #:loc loc))
  #:side-effect-free!)

(define-primitive %%set-box!
  (lambda (loc a value)
    (c:= (c:field a (c:string box-value-property-name)) value #:loc loc)))

(provide %%box
         %%unbox
         %%set-box!)

;;;

(define-primitive wrap-for-callback-procedure
  (lambda (loc continuation-trampoline-var current-toplevel-var)
    (let ([marks 'za] [proc 'zb] [method? 'zc] [this 'zd] [args 'ze] [toplevel 'zf] [result 'zg])
      @js[#:loc loc]{
        function (@marks, @proc, @method?) {
            return function () {
                var @this = this;
                var @args = arguments;
                @marks = {"@cell-property-name": null,
                          "@next-frame-property-name": null}
                if (@method?)
                    Array.prototype.unshift.call(@args, @marks, @this)
                else
                    Array.prototype.unshift.call(@args, @marks)
                var @toplevel = @current-toplevel-var;
                @current-toplevel-var = function () {
                    return @|proc|.apply(@this, @args);
                };
                var @result = @continuation-trampoline-var();
                @current-toplevel-var = @toplevel;
                return @result;
            };
        }}))
  #:fixed-denotation procedure)

(define-primitive %%void
  (lambda (loc) (c:var-ref 'undefined #:loc loc))
  #:fixed-denotation undefined)

(define-primitive %%void?
  (lambda (loc obj) (c:=== obj (c:var-ref 'undefined) #:loc loc))
  #:referencial-transparent!)

(define-primitive empty-function-procedure
  (lambda (loc) (c:fn '()))
  #:fixed-denotation procedure)

(provide wrap-for-callback-procedure
         %%void %%void?
         empty-function-procedure)

;;; Continuation marks

(define-primitive initial-mark-frame
  (lambda (loc)
    @js[#:loc loc]{{"@cell-property-name": null, "@next-frame-property-name": null}}))

(define-primitive continuation-marks-with-extended-top-frame
  (lambda (loc varp keyp valuep)
    (let ([var 'za] [key 'zb] [value 'zc] [new-cell 'zd] [cell 'ze] [key-pn cell-key-property-name] [value-pn cell-value-property-name] [next-pn next-cell-property-name])
      @js[#:loc loc]{
        (function (@var, @key, @value) {
             var @new-cell = { @key-pn : @key, @value-pn : @value, @next-pn : null };
             for (var @cell = @|var|["@cell-property-name"];
                  @cell != null;
                  @cell = @|cell|["@next-pn"])
                 if (@key !== @|cell|["@key-pn"])
                     @new-cell = { @key-pn : @|cell|["@key-pn"], @value-pn : @|cell|["@value-pn"], @next-pn : @new-cell };
             return { @next-frame-property-name : @|var|["@next-frame-property-name"], @cell-property-name : @new-cell };
         })(@varp, @keyp, @valuep)}))
  #:side-effect-free!)

(define-primitive extended-continuation-marks
  (lambda (loc var) @js[#:loc loc]{({ @next-frame-property-name : @var, @cell-property-name : null })})
  #:side-effect-free!)

(define-primitive %%continuation-marks
  (lambda (loc cont)
    @js[#:loc loc]{@|cont|["@continuation-marks-property-name"]})
  #:side-effect-free!)

(define-primitive %%continuation-marks-set!
  (lambda (loc cont marks)
    @js[#:loc loc]{@|cont|["@continuation-marks-property-name"] = @marks}))

(define-primitive current-continuation-marks-procedure
  (lambda (loc)
    (let ([marks 'za] [kont 'zb])
      @js[#:loc loc]{
        function (@marks) {
            return @marks;
        }}))
  #:fixed-denotation procedure)

(define-primitive %%continuation-mark-set->list
  (lambda (loc framep keyp)
    (let ([frame 'za] [key 'zb] [f 'zc] [c 'zd] [l 'ze] [t 'zf])
      @js[#:loc loc]{
        (function (@frame, @key) {
            var @l, @t;
            @l = @t = {};
            for (var @f = @frame;
                 @|f| !== null;
                 @f = @|f|["@next-frame-property-name"])
                for (var @c = @|f|["@cell-property-name"];
                     @|c| !== null;
                     @c = @|c|["@next-cell-property-name"])
                    if (@|c|["@cell-key-property-name"] === @key) {
                        @|t| = @|t|["@cdr-property-name"] = { @car-property-name : @|c|["@cell-value-property-name"], @cdr-property-name : null };
                        break;
                    }
            return @|l|["@cdr-property-name"];
        })(@framep, @keyp)}))
  #:fixed-denotation procedure)

(provide initial-mark-frame
         continuation-marks-with-extended-top-frame
         extended-continuation-marks
         %%continuation-marks %%continuation-marks-set!
         current-continuation-marks-procedure
         %%continuation-mark-set->list)

;;;

(define-primitive protected-procedure
  (lambda (loc continuation-index-var
               continuation-state-var
               store-continuation-exception-var
               tail-object-var tail-trampoline-var tail-trampoline-restart-var)
    (let ([marks 'za] [proc 'zb] [value 'zc])
      (c:fn (list marks proc value)
            (c:var-decl (cons 'zr (c:field tail-object-var tail-object-calls-property-name))
                        'zs)
            (recover-continuation-code continuation-index-var
                                       continuation-state-var
                                       (list 'zr 'zs marks proc value))
            (c:try
             (tail-call-trampoline proc
                                   '()
                                   #:apply? #f
                                   tail-object-var
                                   tail-trampoline-var
                                   tail-trampoline-restart-var
                                   'zr 'zs)
             'zq
             (c:block
              (create-stack-code continuation-state-var
                                 store-continuation-exception-var
                                 'zq
                                 (c:array 'zr 'zs marks proc value))
              (c:if (c:!x (c:instance-of 'zq store-continuation-exception-var))
                    (c:return value))
              (c:throw 'zq))))))
  #:fixed-denotation procedure)

(provide protected-procedure)

;;;

(define-primitive %%object-properties
  (lambda (loc objp)
    (let ([obj 'za] [l 'zb] [p 'zc])
      @js[#:loc loc]{
        (function (@obj) {
             var @l = null;
             for (var @p in @obj)
                 @l = { @car-property-name : @p, @cdr-property-name : @l };
             return @l;
         })(@objp)}))
  #:side-effect-free!)

(define-primitive %%object-own-properties
  (lambda (loc objp)
    (let ([obj 'za] [l 'zb] [p 'zc])
      @js[#:loc loc]{
        (function (@obj) {
            var @l = null;
            for (var @p in @obj)
                if (@|obj|.hasOwnProperty(@p))
                    @l = { @car-property-name : @p, @cdr-property-name : @l };
            return @l;
        })(@objp)}))
  #:side-effect-free!)

(provide
 %%object-properties
 %%object-own-properties)

;;;

(define-primitive %%regexp?
  (lambda (loc obj) @js[#:loc loc]{obj instanceof RegExp})
  #:referencial-transparent!)

(define-primitive %%regexp
  (case-lambda
    [(loc str)   @js[#:loc loc]{new RegExp(@str, "")}]
    [(loc str c) @js[#:loc loc]{new RegExp(@str, @c)}])
  #:referencial-transparent!)

(define-primitive %%regexp-match
  (lambda (loc re str) @js[#:loc loc]{@|str|.match(@re)})
  #:referencial-transparent!)

(define-primitive %%regexp-match?
  (lambda (loc re str) @js[#:loc loc]{@|re|.test(@str)})
  #:referencial-transparent!)

(define-primitive %%regexp-split
  (lambda (loc re str limit) @js[#:loc loc]{@|str|.split(@re, @limit)})
  #:referencial-transparent!)

(define-primitive %%regexp-replace
  (lambda (loc re str1 str2) @js[#:loc loc]{@|str1|.replace(@re, @str2)})
  #:referencial-transparent!)

(define-primitive %%global-regexp
  (lambda (loc rep)
    (let ([re 'za])
      @js[#:loc loc]{
        (function (@re) {
            if (@|re|.global)
                return @re;
            else
                return new RegExp(@|re|.source, @|re|.ignoreCase ? (@|re|.multiline ? "gim" : "gi") : (@|re|.multiline ? "gm" : "g"))
        })(@rep)}))
  #:referencial-transparent!)

(provide %%regexp?
         %%regexp
         %%regexp-match
         %%regexp-match?
         %%regexp-split
         %%regexp-replace
         %%global-regexp)

;;;

(define-primitive %%string->float
  (lambda (loc str) @js[#:loc loc]{parseFloat(@str)})
  #:referencial-transparent!)

(define-primitive %%string->integer
  (lambda (loc str base) @js[#:loc loc]{parseInt(@str, @base)})
  #:referencial-transparent!)

(provide %%string->float
         %%string->integer)

;;;

(define-primitive %%error
  (lambda (loc strp)
    (let ([str 'za])
      @js[#:loc loc]{
        (function (@str) {
             throw new Error (@str);
         })(@strp)})))

(provide %%error)