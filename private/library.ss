#lang s-exp "bootstrap-lang.rkt"

(require
 "../compiler/primitives.rkt"
 "../compiler/primitives2.rkt"
 "bootstrap.ss"
 "bootstrap2.ss"
 "cond.ss")

(provide
 (all-from-out "bootstrap.ss")
 (all-from-out "bootstrap2.ss"))

;;;; 11 Base library

;;; 5 Equivalence predicates

(define (eqv? x y)
  (or (eq? x y)
      (and (number? x) (number? y) (= x y))
      (and (char? x) (char? y) (char=? x y))))

(define (eq? x y)
  (%%eq? x y))

(define (equal? o1 o2)
  (or (eqv? o1 o2)
      (and (pair? o1) (pair? o2)
           (equal? (car o1) (car o2))
           (equal? (cdr o1) (cdr o2)))
      (and (string? o1) (string? o2)
           (string=? o1 o2))
      (and (vector? o1) (vector? o2)
           (let ([len (vector-length o1)])
             (and (= len (vector-length o2))
                  (let loop ([i 0])
                    (cond [(= i len) #t]
                          [(not (equal? (vector-ref o1 i)
                                        (vector-ref o2 i)))
                           #f]
                          [else (loop (add1 i))])))))))

(provide eqv? eq? equal?)

;;; 6 Procedure predicate

(define (procedure? x)
  (%%procedure? x))

(provide procedure?)

;;; 7.4 Arithmetic: Numerical operations

(define (number? x)
  (%%number? x))

(define (complex? x)
  (number? x))

(define (real? x)
  (number? x))

(define (rational? x)
  (and (number? x) (= x (round x))))

(define (integer? x)
  (and (number? x) (= x (round x))))

;; real-valued? rational-valued? integer-valued?

(define (exact? x)   #f)
(define (inexact? x) #t)

;; exact inexact

(define =
  (case-lambda
    [(x y)      (%%= x y)]
    [(x y . zs) (and (%%= x y) (apply = y zs))]))

(define <
  (case-lambda
    [(x y)      (%%< x y)]
    [(x y . zs) (and (%%< x y) (apply < y zs))]))

(define >
  (case-lambda
    [(x y)      (%%< y x)]
    [(x y . zs) (and (%%< y x) (apply > y zs))]))

(define <=
  (case-lambda
    [(x y)      (%%<= x y)]
    [(x y . zs) (and (%%<= x y) (apply <= y zs))]))

(define >=
  (case-lambda
    [(x y)      (%%<= y x)]
    [(x y . zs) (and (%%<= y x) (apply >= y zs))]))

(define (zero? x)
  (= 0 x))

(define (positive? x)
  (< 0 x))

(define (negative? x)
  (< x 0))

(define (odd? x)                        ; FIXME: check that argument is integer
  (= 1 (mod x 2)))

(define (even? x)                       ; FIXME: check that argument is integer
  (= 0 (mod x 2)))

;; finite? infinite? nan?

(define max
  (case-lambda
   [(x)        x]
   [(x y . zs) (apply max (if (> x y) x y) zs)]))

(define min
  (case-lambda
    [(x)        x]
    [(x y . zs) (apply min (if (< x y) x y) zs)]))

(define +
  (case-lambda
    [()         0]
    [(x)        x]
    [(x y . xs) (apply + (%%+ x y) xs)]))

(define *
  (case-lambda
    [()         1]
    [(x)        x]
    [(x y . xs) (apply * (%%* x y) xs)]))

(define -
  (case-lambda
    [(x)        (%%- 0 x)]
    [(x y)      (%%- x y)]
    [(x y . xs) (apply - (%%- x y) xs)]))

(define /
  (case-lambda
    [(x)        (%%/ 1 x)]
    [(x y)      (%%/ x y)]
    [(x y . xs) (apply / (%%/ x y) xs)]))

(define (abs x)
  (if (negative? x) (- x) x))

;; div-and-mod

(define (div x y)
  (let ([div (/ x y)])
    (if (< div 0)
        (ceiling div)
        (floor div))))

(define (mod x y)
  (%%mod x y))

;; div0-and-mod0 div0 mod0

(define (gcd2 x y)
  (cond [(= 0 x) y]
        [(= 0 y) x]
        [else (let ([x (abs x)]
                    [y (abs y)])
                (let loop ([x (max x y)]
                           [y (min x y)])
                  (let ([x (div x y)])
                    (if (zero? x)
                        y
                        (loop y x)))))]))

(define gcd
  (case-lambda
    [()         0]
    [(x)        x]
    [(x y . xs) (apply gcd (gcd2 x y) xs)]))

(define lcm
  (case-lambda
    [()         1]
    [(x)        x]
    [(x y . xs) (apply lcm (abs (/ (* x y) (gcd x y))) xs)]))

;; numerator denominator

(define (floor x)
  (%%floor x))

(define (ceiling x)
  (%%ceiling x))

;; truncate

(define (round x)
  (%%round x))

;; rationalize

(define (exp x)
  (%%exp x))

(define log
  (case-lambda
    [(x) (%%log x)]
    [(x base)
     (if (= base 2)
         (%%log2 x)
         (error 'log "cannot do base" base))]))

(define (sin x)
  (%%sin x))

(define (cos x)
  (%%cos x))

(define (tan x)
  (%%tan x))

(define (asin x)
  (%%asin x))

(define (acos x)
  (%%acos x))

(define atan
  (case-lambda
    [(y)   (%%atan y)]
    [(y x) (%%atan2 y x)]))

(define (sqrt x)
  (%%sqrt x))

;; exact-integer-sqrt

(define (expt x y)
  (%%expt x y))

;; make-rectangular make-polar real-part imag-part magnitude angle

;; FIXME: precision argument handling
(define number->string
  (case-lambda
    [(n)       (%%number->string n 10)]
    [(n radix) (%%number->string n radix)]))

(define string->number
  (let ([re (%%regexp "^-?(?:[0-9]+\\.?|[0-9]*\\.[0-9]+)$")]
        [re2 (%%regexp "^-?[0-1]+$")]
        [re8 (%%regexp "^-?[0-7]+$")]
        [re16 (%%regexp "^-?[0-9a-fA-F]+$")])
    (case-lambda
      [(s) (and (%%regexp-match? re s) (%%string->float s))]
      [(s radix)
       (cond [(= radix 10) (and (%%regexp-match? re s)   (%%string->float s))]
             [(= radix 16) (and (%%regexp-match? re16 s) (%%string->integer s 16))]
             [(= radix 2)  (and (%%regexp-match? re2 s)  (%%string->integer s 2))]
             [(= radix 8)  (and (%%regexp-match? re8 s)  (%%string->integer s 8))]
             [else (error 'string->number "invalid radix" radix)])])))

(provide number? complex? real? rational? integer?
         exact? inexact?
         = < > <= >=
         zero? positive? negative? odd? even?
         max min
         + * - /
         abs
         div mod
         gcd lcm
         floor ceiling round
         exp log sin cos tan asin acos atan
         sqrt
         expt
         number->string
         string->number)

;;; 8 Booleans

(define (not x)
  (eq? x #f))

(define (boolean? obj)
  (%%boolean? obj))

(define boolean=?
  (case-lambda
    [(b1 b2)      (%%boolean=? b1 b2)]
    [(b1 b2 . bs) (apply boolean=? (%%boolean=? b1 b2) bs)]))

(provide not
         boolean?
         boolean=?)

;;; 9 Pairs and lists

(define (pair? obj)
  (%%pair? obj))

(define (cons a d)
  (%%cons a d))

(define (car pair)
  (%%car pair))

(define (cdr pair)
  (%%cdr pair))

(define (caar   p) (car (car p)))
(define (cadr   p) (car (cdr p)))
(define (cdar   p) (cdr (car p)))
(define (cddr   p) (cdr (cdr p)))
(define (caaar  p) (car (car (car p))))
(define (caadr  p) (car (car (cdr p))))
(define (cadar  p) (car (cdr (car p))))
(define (caddr  p) (car (cdr (cdr p))))
(define (cdaar  p) (cdr (car (car p))))
(define (cdadr  p) (cdr (car (cdr p))))
(define (cddar  p) (cdr (cdr (car p))))
(define (cdddr  p) (cdr (cdr (cdr p))))
(define (caaaar p) (car (car (car (car p)))))
(define (caaadr p) (car (car (car (cdr p)))))
(define (caadar p) (car (car (cdr (car p)))))
(define (caaddr p) (car (car (cdr (cdr p)))))
(define (cadaar p) (car (cdr (car (car p)))))
(define (cadadr p) (car (cdr (car (cdr p)))))
(define (caddar p) (car (cdr (cdr (car p)))))
(define (cadddr p) (car (cdr (cdr (cdr p)))))
(define (cdaaar p) (cdr (car (car (car p)))))
(define (cdaadr p) (cdr (car (car (cdr p)))))
(define (cdadar p) (cdr (car (cdr (car p)))))
(define (cdaddr p) (cdr (car (cdr (cdr p)))))
(define (cddaar p) (cdr (cdr (car (car p)))))
(define (cddadr p) (cdr (cdr (car (cdr p)))))
(define (cdddar p) (cdr (cdr (cdr (car p)))))
(define (cddddr p) (cdr (cdr (cdr (cdr p)))))

(define (null? obj)
  (eq? '() obj))

(define (list? x)
  (let race ([h x] [t x])
    (if (pair? h)
        (let ([h (cdr h)])
          (if (pair? h)
              (and (not (eq? h t))
                   (race (cdr h) (cdr t)))
              (null? h)))
        (null? h))))

(define (length list)
  (let loop ([list list] [c 0])
    (if (null? list)
        c
        (loop (cdr list) (add1 c)))))

(define (append . xs)
  (let ([rev (lambda (list tail)
               (let loop ([xs list]
                          [result tail])
                 (if (null? xs)
                     result
                     (loop (cdr xs)
                           (cons (car xs) result)))))])
    (if (null? xs)
        '()
        (let loop ([x (car xs)]
                   [xs (cdr xs)]
                   [acc '()])
          (cond [(null? xs) (rev acc x)]
                [(null? x)
                 (loop (car xs) (cdr xs) acc)]
                [else
                 (loop (cdr x) xs (cons (car x) acc))])))))

(define (reverse list)
  (let loop ([list list]
             [result '()])
    (if (null? list)
        result
        (loop (cdr list)
              (cons (car list) result)))))

(define (list-tail xs k)
  (if (zero? k)
      xs
      (list-tail (cdr xs) (- k 1))))

(define (list-ref xs k)
  (car (list-tail xs k)))

(define (map1 proc list)
  (let loop ([list list] [acc '()])
    (if (null? list)
        (reverse acc)
        (loop (cdr list)
              (cons (proc (car list))
                    acc)))))

(define map
  (case-lambda
    [(proc lst) (map1 proc lst)]
    [(proc l1 l2)
     (let loop ([l1 l1] [l2 l2] [acc '()])
       (if (or (null? l1)
               (null? l2))
           (reverse acc)
           (loop (cdr l1)
                 (cdr l2)
                 (cons (proc (car l1) (car l2))
                       acc))))]
    [(proc . lists)
     (let loop ([lists lists] [acc '()])
       (if (exists null? lists)
           (reverse acc)
           (loop (map1 cdr lists)
                 (cons (apply proc (map1 car lists))
                       acc))))]))

(define (exists proc list)
  (cond [(null? list) #f]
        [(pair? list) (or (proc (car list)) (exists proc (cdr list)))]
        [else (error 'exists "second argument must be a proper list, was ~S" list)]))

(define for-each
  (case-lambda
    [(proc list)
     (let loop [(l list)]
       (if (null? l)
           (void)
           (begin
             (proc (car l))
             (loop (cdr l)))))]
    [(proc list1 list2)
     (let loop [(l1 list1) (l2 list2)]
       (if (or (null? l1) (null? l2))
           (void)
           (begin
             (proc (car l1) (car l2))
             (loop (cdr l1) (cdr l2)))))]
    [(proc . lists)
     (let loop ([lists lists])
       (if (exists null? lists)
           (void)
           (begin
             (apply proc (map car lists))
             (loop (map cdr lists)))))]))

(provide pair? cons car cdr
         caar cdar cadr cddr caaar cdaar cadar cddar caadr cdadr caddr cdddr
         caaaar cdaaar cadaar cddaar caadar cdadar caddar cdddar
         caaadr cdaadr cadadr cddadr caaddr cdaddr cadddr cddddr
         null?
         list?
         length
         append
         reverse
         list-tail
         list-ref
         map
         for-each
         exists ;; used on rnrs/list-utilities.scm
         )

;;; 10 Symbols

(define (symbol? obj)
  (%%symbol? obj))

(define (symbol->string sym)
  (%%symbol->string sym))

(define symbol=?
  (case-lambda
    [(b1 b2)      (%%symbol=? b1 b2)]
    [(b1 b2 . bs) (apply symbol=? (%%symbol=? b1 b2) bs)]))

(define (string->symbol str)
  (%%string->symbol str))

(provide symbol?
         symbol->string
         symbol=?
         string->symbol)

;;; 11 Characters

(define (char? obj)
  (%%char? obj))

(define (char->integer char)
  (%%char->integer char))

(define (integer->char int)
  (%%integer->char int))

(define char=?
  (case-lambda
    [(x y)      (%%char=? x y)]
    [(x y . zs) (and (%%char=? x y) (apply char=? y zs))]))

(define char<?
  (case-lambda
    [(x y)      (%%char<? x y)]
    [(x y . zs) (and (%%char<? x y) (apply char<? y zs))]))

(define char>?
  (case-lambda
    [(x y)      (%%char<? y x)]
    [(x y . zs) (and (%%char<? y x) (apply char>? y zs))]))

(define char<=?
  (case-lambda
    [(x y)      (%%char<=? x y)]
    [(x y . zs) (and (%%char<=? x y) (apply char<=? y zs))]))

(define char>=?
  (case-lambda
    [(x y)      (%%char<=? y x)]
    [(x y . zs) (and (%%char<=? y x) (apply char>=? y zs))]))

(provide char?
         char->integer integer->char
         char=? char<? char>? char<=? char>=?)

;;; 12 Strings

(define (string? obj)
  (%%string? obj))

(define make-string
  (case-lambda
    [(k)      (%%make-string k #\nul)]
    [(k char) (%%make-string k char)]))

(define (string . xs)
  (list->string xs))

(define (string-length str)
  (%%string-length str))

(define (string-ref str index)
  (%%string-ref str index))

(define string=?
  (case-lambda
    [(x y)      (%%string=? x y)]
    [(x y . zs) (and (%%string=? x y) (apply string=? y zs))]))

(define string<?
  (case-lambda
    [(x y)      (%%string<? x y)]
    [(x y . zs) (and (%%string<? x y) (apply string<? y zs))]))

(define string>?
  (case-lambda
    [(x y)      (%%string<? y x)]
    [(x y . zs) (and (%%string<? y x) (apply string>? y zs))]))

(define string<=?
  (case-lambda
    [(x y)      (%%string<=? x y)]
    [(x y . zs) (and (%%string<=? x y) (apply string<=? y zs))]))

(define string>=?
  (case-lambda
    [(x y)      (%%string<=? y x)]
    [(x y . zs) (and (%%string<=? y x) (apply string>=? y zs))]))

(define substring
  (case-lambda
    [(str start)     (%%substring str start)]
    [(str start end) (%%substring str start end)]))

(define string-append
  (case-lambda
    [()         ""]
    [(x)        x]
    [(x y . xs) (apply string-append (%%string-append x y) xs)]))

(define (string->list string)
  (let loop ([i (- (string-length string) 1)]
             [lst '()])
    (if (= -1 i)
        lst
        (loop (sub1 i)
              (cons (string-ref string i)
                    lst)))))

(define (list->string lst)
  (%%list->string lst))

(define (string-for-each proc string . more-strings)
  (apply for-each proc (string->list string) (map string->list more-strings)))

(define (string-copy string)
  string)

(provide string?
         make-string
         string
         string-length
         string-ref
         string=?
         string<? string>? string<=? string>=?
         substring
         string-append
         string->list list->string
         string-for-each
         string-copy)

;;; 13 Vectors

(define (vector? obj)
  (%%vector? obj))

(define make-vector
  (case-lambda
    [(k)      (%%make-vector k)]
    [(k fill) (let ([v (%%make-vector k)])
                (vector-fill! v fill)
                v)]))

(define (vector . xs)
  (list->vector xs))

(define (vector-length vec)
  (%%vector-length vec))

(define (vector-ref vec k)
  (%%vector-ref vec k))

(define (vector-set! vec k obj)
  (%%vector-set! vec k obj))

(define (vector->list vector)
  (let loop ([i (sub1 (vector-length vector))]
             [xs '()])
    (if (negative? i)
        xs
        (loop (sub1 i)
              (cons (vector-ref vector i) xs)))))

(define (list->vector xs)
  (let* ([len (length xs)]
         [vec (make-vector len)])
    (let loop ([xs xs]
               [i 0])
      (cond [(null? xs) vec]
            [else
             (vector-set! vec i (car xs))
             (loop (cdr xs) (add1 i))]))))

(define (vector-fill! vec fill)
  (let ([len (vector-length vec)])
    (let loop ([i 0])
      (cond [(= i len) vec]
            [else
             (vector-set! vec i fill)
             (loop (add1 i))]))))

(define (vector-map1 proc vec)
  (let ([len (vector-length vec)])
    (let ([new-vec (make-vector len)])
      (let loop ([i 0])
        (cond [(= len i) new-vec]
              [else
               (vector-set! new-vec i (proc (vector-ref vec i)))
               (loop (add1 i))])))))

(define vector-map
  (case-lambda
    [(proc vec) (vector-map1 proc vec)]
    [(proc vec1 vec2)
     (let ([len (vector-length vec1)])
       (let ([new-vec (make-vector len)])
         (let loop ([i 0])
           (cond [(= len i) new-vec]
                 [else
                  (vector-set! new-vec i (proc (vector-ref vec1 i) (vector-ref vec2 i)))
                  (loop (add1 i))]))))]
    [(proc . vecs)
     (let ([len (vector-length (car vecs))])
       (let ([new-vec (make-vector len)])
         (let loop ([i 0])
           (cond [(= len i) new-vec]
                 [else
                  (vector-set! new-vec i (apply proc (map (lambda (v) (vector-ref v i)) vecs)))
                  (loop (add1 i))]))))]))

(define vector-for-each
  (case-lambda
    [(proc vec)
     (let ([len (vector-length vec)])
       (let loop ([i 0])
         (if (= len i)
             (void)
             (begin
               (proc (vector-ref vec i))
               (loop (add1 i))))))]
    [(proc vec1 vec2)
     (let ([len (vector-length vec1)])
       (let loop ([i 0])
         (if (= len i)
             (void)
             (begin
               (proc (vector-ref vec1 i) (vector-ref vec2 i))
               (loop (add1 i))))))]
    [(proc . vecs)
     (let ([len (vector-length (car vecs))])
       (let loop ([i 0])
         (if (= len i)
             (void)
             (begin
               (apply proc (map (lambda (v) (vector-ref v i)) vecs))
               (loop (add1 i))))))]))

(provide vector?
         make-vector
         vector
         vector-length
         vector-ref
         vector-set!
         vector->list list->vector
         vector-fill!
         vector-map
         vector-for-each)

;;; 14 Errors and violations

(define (error who message . irritants)
  (es-throw (es-new (es-global "Error") (es-call (es-property (es-global "JSON") "stringify") (vector (symbol->string who) message irritants)))))

(define (assertion-violation who message . irritants)
  (apply error who message irritants))

(define with-exception-handler
  (with-exception-handler-procedure continuation-index continuation-state store-continuation-exception
                                    switch-continuation-exception
                                    tail-object tail-trampoline tail-trampoline-restart))

;; assert

(provide error assertion-violation
         with-exception-handler)

;;; 15 Control features

(define apply (apply-procedure continuation-index continuation-state store-continuation-exception
                               tail-object tail-trampoline tail-trampoline-restart))

(define call-with-values
  (call-with-values-procedure continuation-index continuation-state store-continuation-exception
                              tail-object tail-trampoline tail-trampoline-restart))

(define %%call/cc
  (call/cc-procedure continuation-index continuation-state store-continuation-exception
                     switch-continuation-exception current-toplevel
                     tail-object tail-trampoline tail-trampoline-restart))

;; This code is from The Scheme Programming Language 3rd ed.
(begin
  (define *winders* '())

  (define call/cc
    (let ([common-tail
           (lambda (x y)
             (let ([lx (length x)] [ly (length y)])
               (let loop ([x (if (> lx ly) (list-tail x (- lx ly)) x)]
                          [y (if (> ly lx) (list-tail y (- ly lx)) y)])
                 (if (eq? x y)
                     x
                     (loop (cdr x) (cdr y))))))])
      (define (do-wind new)
        (let ([tail (common-tail new *winders*)])
          (let f ([l *winders*])
            (if (eq? l tail)
                (void)
                (begin
                  (set! *winders* (cdr l))
                  ((cdar l))
                  (f (cdr l)))))
          (let f ([l new])
            (if (eq? l tail)
                (void)
                (begin
                  (f (cdr l))
                  ((caar l))
                  (set! *winders* l))))))
      (lambda (f)
        (%%call/cc
         (lambda (k)
           (f (let ([save *winders*])
                ;; Hack to pass the continuation marks
                (define k2
                  (lambda x
                    (if (eq? save *winders*)
                        (void)
                        (do-wind save))
                    (apply k x)))
                (%%continuation-marks-set! k2 (%%continuation-marks k))
                k2)))))))

  (define (dynamic-wind in body out)
    (in)
    (set! *winders* (cons (cons in out) *winders*))
    (call-with-values body
      (lambda ans
        (set! *winders* (cdr *winders*))
        (out)
        (apply values ans)))))

;; (define (values . things)
;;   (call/cc (lambda (k) (apply k things))))

(define values
  (case-lambda
    [()  (%%values #())]
    [(x) x]
    [xs  (%%values (list->vector xs))]))

(provide apply
         call/cc
         (rename-out (call/cc call-with-current-continuation))
         values
         call-with-values
         dynamic-wind)

;;; Continuation marks

(define (continuation-marks cont) ;; Missing argument: prompt tag
  (%%continuation-marks cont))

(define current-continuation-marks ;; Missing argument: prompt tag
  (current-continuation-marks-procedure))

(define (continuation-mark-set->list marks key)
  (%%continuation-mark-set->list marks key))

(provide continuation-marks
         current-continuation-marks
         continuation-mark-set->list)

;;;

(define (box x)
  (%%box x))

(define (unbox x)
  (%%unbox x))

(define (set-box! x value)
  (%%set-box! x value))

(define (void? obj)
  (%%void? obj))

(define (add1 a)
  (%%+ a 1))

(define (sub1 a)
  (%%- a 1))

(define null '())

(define abort-current-continuation
  (abort-current-continuation-procedure switch-continuation-exception))

(define (suspend proc)
  (call/cc (lambda (k)
             (proc k)
             (abort-current-continuation))))

(define wrap-for-callback
  (wrap-for-callback-procedure continuation-trampoline current-toplevel))

(define-syntax-rule (assert expr)
  (if expr
      (void)
      (error 'assert "assertion failed" 'expr)))

(define (identity obj)
  obj)

(define (constantly obj)
  (lambda (_) obj))

(define-syntax-rule (let/cc k expr1 expr2 ...)
  (call/cc (lambda (k) expr1 expr2 ...)))

(define (make-promise p)
  (let ([vals #f] [set? #f])
    (lambda ()
      (if set?
          (apply values vals)
          (call-with-values p
            (lambda x
              (set! vals x)
              (set! set? #t)
              (apply values x)))))))

(define-syntax-rule (delay expr)
  (make-promise (lambda () expr)))

(define (force promise)
  (promise))

(provide box unbox set-box!
         void?
         add1 sub1
         null
         abort-current-continuation
         suspend
         wrap-for-callback
         assert identity constantly
         let/cc
         delay force)

(define (es-object-properties obj)
  (%%object-properties obj))

(define (es-object-own-properties obj)
  (%%object-own-properties obj))

(provide es-object-properties
         es-object-own-properties)

(define bitwise-ior
  (case-lambda
    [()         0]
    [(x)        x]
    [(x y . xs) (apply bitwise-ior (%%bitwise-ior x y) xs)]))

(define bitwise-and
  (case-lambda
    [()         -1]
    [(x)        x]
    [(x y . xs) (apply bitwise-and (%%bitwise-and x y) xs)]))

(define bitwise-xor
  (case-lambda
    [()         0]
    [(x)        x]
    [(x y . xs) (apply bitwise-xor (%%bitwise-xor x y) xs)]))

(define (bitwise-not x)
  (%%bitwise-not x))

(define (arithmetic-shift n m)
  (if (negative? m)
      (%%shift-right n (- m))
      (%%shift-left n m)))

(define protected (protected-procedure continuation-index
                                       continuation-state
                                       store-continuation-exception
                                       tail-object tail-trampoline tail-trampoline-restart))

(provide bitwise-ior
         bitwise-and
         bitwise-xor
         bitwise-not
         arithmetic-shift
         protected)
