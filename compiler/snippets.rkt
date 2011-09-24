#lang scheme/base

(require scheme/port
         scheme/string)

(require (prefix-in d: "../externals/javascript/parse.ss")
         (prefix-in d: "../externals/javascript/print.ss")
         (prefix-in ast: "../externals/javascript/ast.ss")
         (prefix-in d: (planet dherman/pprint)))

(require "syntax.rkt"
         "backend.rkt"
         (prefix-in c: "ecmascript.rkt"))

;;; Property names

(define +property-hash+
  (make-hash '((box-value          . "$b")
               (car                . "$a")
               (cdr                . "$d")
               (char->integer      . "$c")
               (continuation-marks . "$m")
               (values             . "$v"))))

(define (find-property-name id)
  (hash-ref +property-hash+ id (lambda () #f)))

(define (set-find-property-name! id property-name)
  (hash-set! +property-hash+ id property-name))

(define next-property-index
  (let ([i 0])
    (lambda ()
      (begin0 i
        (set! i (add1 i))))))

(define (property-name id)
  (or (find-property-name id)
      (let ([property-name (string-append "p" (number->string (next-property-index) 16))])
        (set-find-property-name! id property-name)
        property-name)))

(provide property-name
         set-find-property-name!
         +property-hash+)

(define box-value-property-name (property-name 'box-value))
(define car-property-name (property-name 'car))
(define cdr-property-name (property-name 'cdr))
(define char->integer-property-name (property-name 'char->integer))
(define original-exception-property-name (property-name 'original-exception))
(define next-procedure-property-name (property-name 'next-procedure))
(define abort-continuation?-property-name (property-name 'abort-continuation?))
(define continuation-marks-property-name (property-name 'continuation-marks))
(define values-property-name (property-name 'values))

(define tail-trampoline-proc-property-name (property-name 'tail-trampoline-proc))
(define tail-trampoline-args-property-name (property-name 'tail-trampoline-args))
;; (define tail-object-proc-property-name (property-name 'tail-object-proc))
(define tail-object-calls-property-name (property-name 'tail-object-calls))

(define cell-property-name       (property-name 'cell))
(define cell-key-property-name   (property-name 'cell-key))
(define cell-value-property-name (property-name 'cell-value))
(define next-cell-property-name  (property-name 'next-cell))
(define next-frame-property-name (property-name 'next-frame))

(provide box-value-property-name
         car-property-name
         cdr-property-name
         char->integer-property-name
         original-exception-property-name
         next-procedure-property-name
         abort-continuation?-property-name
         continuation-marks-property-name

         values-property-name

         tail-trampoline-proc-property-name
         tail-trampoline-args-property-name
         ;; tail-object-proc-property-name
         tail-object-calls-property-name

         cell-property-name
         cell-key-property-name
         cell-value-property-name
         next-cell-property-name
         next-frame-property-name
         )

;;;

(define (js #:loc [loc #f] . strings)
  (let ([all-strings (map (lambda (x)
                            (if (string? x)
                                x
                                (d:pretty-format
                                 (d:format-expression
                                  (c:->expression
                                   x)))))
                          strings)])
    (let ([str (string-append* all-strings)])
      (with-handlers ([exn:fail? (lambda (v)
                                   (error (format "error parsing ~S" str)))])
        (call-with-input-string str d:parse-expression)))))

(define (recover-continuation-code continuation-index-var
                                   continuation-state-var
                                   variables
                                   #:if-not-recovering [if-not-recovering (c:empty)])
  (c:if (c:< 0 continuation-index-var)
        (if (backend-has-array-comprehension? (current-backend))
            (c:= (apply c:array variables)
                 (c:field continuation-state-var (c:--x continuation-index-var)))
            (apply c:block
                   (c:var-decl (cons 'zo
                                     (c:field continuation-state-var
                                              (c:--x continuation-index-var))))
                   (for/list ([i (in-range (length variables))]
                              [v variables])
                     (c:= v (c:field 'zo i)))))
        if-not-recovering))

(define (create-stack-code continuation-state-var
                           store-continuation-exception-var
                           exc-var
                           variables-array)
  (c:if (c:instance-of exc-var store-continuation-exception-var)
        (c:call (c:field continuation-state-var "push") variables-array)))

(define (tail-call-trampoline1 op args tail-object trampoline tail-trampoline-restart tail-calls tmp #:apply? [apply? #f])
  (let ([calls tail-object-calls-property-name])
    (c:block
     (if apply?
         #f
         (let ([tmp (apply c:array args)])
           (set! args 'zz)
           (c:var-decl (cons args tmp)))) ;; HACK: we should use a generated name instead of 'zz
     (if (or apply?
              (not (ast:FunctionExpression? op)))
         #f
         (let ([tmp op])
           (set! op 'zy)
           (c:var-decl (cons op tmp))))
     (c:if (c:=== (c:this) tail-object)
           (c:return (if apply?
                         (apply c:new trampoline op args)
                         (c:new trampoline op args))) ;; HACK
           (c:block
            (c:if (c:!x (c:instance-of tmp trampoline))
                  (c:= tmp (if apply?
                               (apply c:call (c:field op "apply") tail-object args)
                               (c:call (c:field op "apply") tail-object args)))) ;; HACK
            (c:return (c:if-e (c:instance-of tmp trampoline)
                              (c:call tail-trampoline-restart tmp)
                              tmp)))))
    ;;   if (this === @tail-object) {
    ;;       return new Trampoline(op, [@args]);
    ;;   } else {
    ;;       @|tail-object|.f = @op;
    ;;       var @tmp = @|tail-object|.f(@args);
    ;;       if (tmp instanceof Trampoline)
    ;;           return @|tmp|.restart();
    ;;       else
    ;;           return @tmp;
    ;;   }}
    ))

(define (tail-call-trampoline op args tail-object trampoline tail-trampoline-restart tail-calls tmp #:apply? [apply? #f])
  (let ([calls tail-object-calls-property-name])
    (c:block
     (if apply?
         #f
         (let ([tmp (apply c:array args)])
           (set! args 'zz)
           (c:var-decl (cons args tmp)))) ;; HACK: we should use a generated name instead of 'zz
     (if (or apply?
              (not (ast:FunctionExpression? op)))
         #f
         (let ([tmp op])
           (set! op 'zy)
           (c:var-decl (cons op tmp))))
     (c:if (c:=== (c:this) tail-object)
           (c:if (c:=== tail-calls max-tail-calls)
                 (c:return (if apply?
                               (apply c:new trampoline op args)
                               (c:new trampoline op args))) ;; HACK
                 (c:block
                  (c:= (c:field tail-object calls) (c:+ tail-calls 1))
                  (c:return (if apply?
                                (apply c:call (c:field op "apply") tail-object args)
                                (c:call (c:field op "apply") tail-object args))))) ;; HACK
           (c:block
            (c:= (c:field tail-object calls) 1)
            (c:if (c:!x (c:instance-of tmp trampoline))
                  (c:= tmp (if apply?
                               (apply c:call (c:field op "apply") tail-object args)
                               (c:call (c:field op "apply") tail-object args)))) ;; HACK
            (c:return (c:if-e (c:instance-of tmp trampoline)
                              (c:call tail-trampoline-restart tmp)
                              tmp)))))
    ;; @js[#:loc loc]{
    ;;   if (this === @tail-object) {
    ;;       if (@tail-calls === @max-tail-calls)
    ;;           return new Trampoline(op, [@args]);
    ;;       else {
    ;;           @|tail-object|.calls = @tail-calls + 1;
    ;;           return @|op|.apply(@|tail-object|, @args)
    ;;       }
    ;;   } else {
    ;;       @|tail-object|.call = 1;
    ;;       var @tmp = @|op|.apply(@tail-object, @args);
    ;;       if (tmp instanceof Trampoline)
    ;;           return @|tmp|.restart();
    ;;       else
    ;;           return @tmp;
    ;;   }}
    ))

(define tail-call-trampoline-aux
  (if single-tail-call? tail-call-trampoline1 tail-call-trampoline))

(provide js
         create-stack-code
         recover-continuation-code
         (rename-out [tail-call-trampoline-aux tail-call-trampoline]))
