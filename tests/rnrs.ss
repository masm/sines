#lang planet masm/sines

(require
 (rename-in "../test.ss"
            [check-expect ->e]
            [check-expect-one-of ->e/one-of]
            [check-quoted-expect ->]
            [check-quoted-expect-one-of ->/one-of]))

;;; 2. Definitions

;;;; 2.1. Variable definitions

(define add3
  (lambda (x) (+ x 3)))
[(add3 3)                                      . -> .  6]

(define first car)
[(first '(1 2))                                . -> .  1]

;;;; 2.2. Syntax definitions

[(let ()
   (define even?
     (lambda (x)
       (or (= x 0) (odd? (- x 1)))))
   (define-syntax odd?
     (syntax-rules ()
       ((odd?  x) (not (even? x)))))
   (even? 10))                                 . -> .  #t]

[(let ()
   (define-syntax bind-to-zero
     (syntax-rules ()
       [(bind-to-zero id) (define id 0)]))
   (bind-to-zero x)
   x)                                          . -> .  0]

;;; 3. Bodies

(let ([x 5])
  (define foo (lambda (y) (bar x y)))
  (define bar (lambda (a b) (+ (* a b) a)))
  [(foo (+ x 3))                              . -> .  45])

;;; 4. Expressions

;;;; 4.1. Quotation

[(quote a)                                     . -> .  a]
[(quote #(a b c))                              . -> .  #(a b c)]
[(quote (+ 1 2))                               . -> .  (+ 1 2)]

['"abc"                                        . -> .  "abc"]
['145932                                       . -> .  145932]
['a                                            . -> .  a]
['#(a b c)                                     . -> .  #(a b c)]
['()                                           . -> .  ()]
['(+ 1 2)                                      . -> .  (+ 1 2)]
['(quote a)                                    . -> .  (quote a)]
[''a                                           . -> .  (quote a)]

;;;; 4.2. Procedures

[((lambda (x) (+ x x)) 4)                      . -> .  8]
[((lambda (x)
     (define (p y)
       (+ y 1))
     (+ (p x) x))
   5)                                           . -> .  11]

(define reverse-subtract
  (lambda (x y) (- y x)))
[(reverse-subtract 7 10)                       . -> .  3]

(define add4
  (let ([x 4])
    (lambda (y) (+ x y))))
[(add4 6)                                      . -> .  10]

[((lambda x x) 3 4 5 6)                        . -> .  (3 4 5 6)]
[((lambda (x y . z) z) 3 4 5 6)                . -> .  (5 6)]

;;;; 4.3. Conditionals

[(if (> 3 2) 'yes 'no)                         . -> .  yes]
[(if (> 2 3) 'yes 'no)                         . -> .  no]
[(if (> 3 2) (- 3 2) (+ 3 2))                  . -> .  1]
;; [(if #f #f)                                    . ->e .  (void)]

;;;; 4.4. Assignments

(let ([x 2])
  [(+ x 1)                                    . -> .  3]
  (set! x 4)
  [(+ x 1)                                    . -> .  5])

;;;; 4.5. Derived conditionals

[(cond [(> 3 2) 'greater]
       [(< 3 2) 'less])                        . -> .  greater]
[(cond [(> 3 3) 'greater]
       [(< 3 3) 'less]
       [else 'equal])                          . -> .  equal]
[(cond ['(1 2 3) => cadr]
       [else #f])                              . -> .  2]

[(case (* 2 3)
   [(2 3 5 7) 'prime]
   [(1 4 6 8 9) 'composite])                   . -> .  composite]
[(case (car '(c d))
   [(a) 'a]
   [(b) 'b])                                   . ->e .  (void)]
[(case (car '(c d))
   [(a e i o u) 'vowel]
   [(w y) 'semivowel]
   [else 'consonant])                          . -> .  consonant]

[(and (= 2 2) (> 2 1))                         . -> .  #t]
[(and (= 2 2) (< 2 1))                         . -> .  #f]
[(and 1 2 'c '(f g))                           . -> .  (f g)]
[(and)                                         . -> .  #t]

[(or (= 2 2) (> 2 1))                          . -> .  #t]
[(or (= 2 2) (< 2 1))                          . -> .  #t]
[(or #f #f #f)                                 . -> .  #f]
[(or '(b c) (/ 3 0))                           . -> .  (b c)]

;;;; 4.6. Binding constructs

[(let ([x 2] (y 3))
   (* x y))                                    . -> .  6]
[(let ([x 2] (y 3))
   (let ([x 7]
         [z (+ x y)])
     (* z x)))                                 . -> .  35]

[(let ([x 2] [y 3])
   (let* ([x 7]
          [z (+ x y)])
     (* z x)))                                 . -> .  70]

[(letrec ([even?
           (lambda (n)
             (if (zero? n)
                 #t
                 (odd? (- n 1))))]
          [odd?
           (lambda (n)
             (if (zero? n)
                 #f
                 (even? (- n 1))))])
   (even? 88))                                 . -> .  #t]

;; [(letrec* ((p (lambda (x)
;;                 (+ 1 (q (- x 1)))))
;;            (q (lambda (y)
;;                 (if (zero? y)
;;                     0
;;                     (+ 1 (p (- y 1))))))
;;            (x (p 5))
;;            (y x))
;;           y)                                          . -> .  5]


[(let-values (((a b) (values 1 2))
              ((c d) (values 3 4)))
   (list a b c d))                             . -> .  (1 2 3 4)]
;; [(let-values (((a b . c) (values 1 2 3 4)))
;;    (list a b c))                               . -> .  (1 2 (3 4))]
[(let ((a 'a) (b 'b) (x 'x) (y 'y))
   (let-values (((a b) (values x y))
                ((x y) (values a b)))
     (list a b x y)))                          . -> .  (x y a b)]


[(let ((a 'a) (b 'b) (x 'x) (y 'y))
   (let*-values (((a b) (values x y))
                 ((x y) (values a b)))
     (list a b x y)))                                    . -> .  (x y x y)]

;;;; 4.7. Sequencing

(define x 0)
[(begin
    (set! x 5)
    (+ x 1))                               . -> .  6]
;; [(begin
;;    (display "4 plus 1 equals ")
;;    (display (+ 4 1))                      ) . -> . ]

;;; 5. Equivalence predicates

[(eqv? 'a 'a)                                  . -> .  #t]
[(eqv? 'a 'b)                                  . -> .  #f]
[(eqv? 2 2)                                    . -> .  #t]
[(eqv? '() '())                                . -> .  #t]
[(eqv? 100000000 100000000)                    . -> .  #t]
[(eqv? (cons 1 2) (cons 1 2))                  . -> .  #f]
[(eqv? (lambda () 1)
       (lambda () 2))                          . -> .  #f]
[(eqv? #f 'nil)                                . -> .  #f]
(define gen-counter
  (lambda ()
    (let ([n 0])
      (lambda () (set! n (+ n 1)) n))))
[(eqv? (gen-counter) (gen-counter))            . -> .  #f]
[(letrec ([f (lambda () (if (eqv? f g) 'f 'both))]
          [g (lambda () (if (eqv? f g) 'g 'both))])
   (eqv? f g))                           . -> .  #f]
[(let ([x '(a)])
    (eqv? x x))                                 . -> .  #t]


[(eq? 'a 'a)                                   . -> .  #t]
[(eq? (list 'a) (list 'a))                     . -> .  #f]
[(eq? '() '())                                 . -> .  #t]

[(eq? car car)                                 . -> .  #t]
[(let ([x '(a)])
   (eq? x x))                                  . -> .  #t]


[(equal? 'a 'a)                                . -> .  #t]
[(equal? '(a) '(a))                            . -> .  #t]
[(equal? '(a (b) c)
         '(a (b) c))                           . -> .  #t]
[(equal? "abc" "abc")                          . -> .  #t]
[(equal? 2 2)                                  . -> .  #t]
[(equal? (make-vector 5 'a)
         (make-vector 5 'a))                   . -> .  #t]
;; [(equal? '#vu8(1 2 3 4 5)
;;          (u8-list->bytevector '(1 2 3 4 5)))   . -> .  #t]
[(let* ([x (list 'a)]
        [y (list 'a)]
        [z (list x y)])
   (list (equal? z (list y x))
         (equal? z (list x x))))               . -> .  (#t #t)]

;;; 6. Procedure predicate

[(procedure? car)                              . -> .  #t]
[(procedure? 'car)                             . -> .  #f]
[(procedure? (lambda (x) (* x x)))             . -> .  #t]
[(procedure? '(lambda (x) (* x x)))            . -> .  #f]

;;; 7. Arithmetic

;; FIXME TODO XXX

;;; 8. Booleans

[(not #t)                                      . -> .  #f]
[(not 3)                                       . -> .  #f]
[(not (list 3))                                . -> .  #f]
[(not #f)                                      . -> .  #t]
[(not '())                                     . -> .  #f]
[(not (list))                                  . -> .  #f]
[(not 'nil)                                    . -> .  #f]


[(boolean? #f)                                 . -> .  #t]
[(boolean? 0)                                  . -> .  #f]
[(boolean? '())                                . -> .  #f]

;;; 9. Pairs and lists

[(pair? '(a . b))                              . -> .  #t]
[(pair? '(a b c))                              . -> .  #t]
[(pair? '())                                   . -> .  #f]
[(pair? '#(a b))                               . -> .  #f]

[(cons 'a '())                                 . -> .  (a)]
[(cons '(a) '(b c d))                          . -> .  ((a) b c d)]
[(cons "a" '(b c))                             . -> .  ("a" b c)]
[(cons 'a 3)                                   . -> .  (a . 3)]
[(cons '(a b) 'c)                              . -> .  ((a b) . c)]

[(car '(a b c))                                . -> .  a]
[(car '((a) b c d))                            . -> .  (a)]
[(car '(1 . 2))                                . -> .  1]
;; [(car '())                                     . -> .  &assertion]

[(cdr '((a) b c d))                            . -> .  (b c d)]
[(cdr '(1 . 2))                                . -> .  2]
;; [(cdr '())                                     . -> .  &assertion]

[(list? '(a b c))                              . -> .  #t]
[(list? '())                                   . -> .  #t]
[(list? '(a . b))                              . -> .  #f]

[(list 'a (+ 3 4) 'c)                          . -> .  (a 7 c)]
[(list)                                        . -> .  ()]

[(length '(a b c))                             . -> .  3]
[(length '(a (b) (c d e)))                     . -> .  3]
[(length '())                                  . -> .  0]

[(append '(x) '(y))                            . -> .  (x y)]
[(append '(a) '(b c d))                        . -> .  (a b c d)]
[(append '(a (b)) '((c)))                      . -> .  (a (b) (c))]
[(append '(a b) '(c . d))                      . -> .  (a b c . d)]
[(append '() 'a)                               . -> .  a]

[(reverse '(a b c))                            . -> .  (c b a)]
[(reverse '(a (b c) d (e (f))))                . -> .  ((e (f)) d (b c) a)]

[(list-tail '(a b c d) 2)                      . -> .  (c d)]

[(list-ref '(a b c d) 2)                       . -> .  c]

[(map cadr '((a b) (d e) (g h)))               . -> .  (b e h)]
[(map (lambda (n) (expt n n))
      '(1 2 3 4 5))                            . -> .  (1 4 27 256 3125)]
[(map + '(1 2 3) '(4 5 6))                     . -> .  (5 7 9)]
[(let ([count 0])
   (map (lambda (ignored)
          (set! count (+ count 1))
          count)
        '(a b)))                               . ->/one-of .  (1 2) (2 1)]

[(let ([v (make-vector 5)])
   (for-each (lambda (i)
               (vector-set! v i (* i i)))
             '(0 1 2 3 4))
   v)                                          . -> . #(0 1 4 9 16)]

;;; 10. Symbols

[(symbol? 'foo)                                . -> .  #t]
[(symbol? (car '(a b)))                        . -> .  #t]
[(symbol? "bar")                               . -> .  #f]
[(symbol? 'nil)                                . -> .  #t]
[(symbol? '())                                 . -> .  #f]
[(symbol? #f)                                  . -> .  #f]

[(symbol->string 'flying-fish)                 . -> .  "flying-fish"]
[(symbol->string 'Martin)                      . -> .  "Martin"]
[(symbol->string (string->symbol "Malvina"))   . -> .  "Malvina"]

[(eq? 'mISSISSIppi 'mississippi)               . -> .  #f]
[(string->symbol "mISSISSIppi")                . -> .  mISSISSIppi]
[(eq? 'bitBlt (string->symbol "bitBlt"))       . -> .  #t]
[(eq? 'JollyWog
      (string->symbol
       (symbol->string 'JollyWog)))            . -> .  #t]
[(string=? "K. Harper, M.D."
           (symbol->string
            (string->symbol "K. Harper, M.D."))) . -> .  #t]

;;; 11. Characters

[(integer->char 32)                            . -> .  #\space]
[(char->integer (integer->char 5000))          . -> .  5000]
;; [(integer->char #\xD800)                       . -> .  &assertion]

[(char<? #\z #\ß)                              . -> .  #t]
[(char<? #\z #\Z)                              . -> .  #f]

;;; 12. Strings

[(string=? "Straße" "Strasse")                 . -> .  #f]

[(string<? "z" "ß")                            . -> .  #t]
[(string<? "z" "zz")                           . -> .  #t]
[(string<? "z" "Z")                            . -> .  #f]

;;; 13. Vectors

[#(0 (2 2 2 2) "Anna")                        . -> .  #(0 (2 2 2 2) "Anna")]

[(vector 'a 'b 'c)                             . -> .  #(a b c)]

[(vector-ref '#(1 1 2 3 5 8 13 21) 5)          . -> .  8]

[(let ([vec (vector 0 '(2 2 2 2) "Anna")])
   (vector-set! vec 1 '("Sue" "Sue"))
   vec)                                        . -> .  #(0 ("Sue" "Sue") "Anna")]
;; [(vector-set! '#(0 1 2) 1 "doe")               . -> .  &assertion]

[(vector->list '#(dah dah didah))              . -> .  (dah dah didah)]

[(list->vector '(dididit dah))                 . -> .  #(dididit dah)]

;;; 14. Errors and violations

;; (define (fac n)
;;   (if (not (integer-valued? n))
;;       (assertion-violation 'fac "non-integral argument" n))
;;   (if (negative? n)
;;       (assertion-violation 'fac "negative argument" n))
;;   (letrec ((loop (lambda (n r)
;;                    (if (zero? n)
;;                        r
;;                        (loop (- n 1) (* r n))))))
;;     (loop n 1)))

;; [(fac 5)                                       . -> .  120]
;; [(fac 4.5)                                     . -> .  &assertion]
;; [(fac -3)                                      . -> .  &assertion]

;;; 15. Control features

[(apply + (list 3 4))                          . -> .  7]

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

[((compose sqrt *) 12 75)                      . -> .  30]

[(call-with-current-continuation
   (lambda (exit)
     (for-each (lambda (x)
                 (when (negative? x)
                   (exit x)))
               '(54 0 37 -3 245 19))
     #t))                                       . -> .  -3]
(define list-length
  (lambda (obj)
    (call-with-current-continuation
     (lambda (return)
       (letrec ([r
                 (lambda (obj)
                   (cond [(null? obj) 0]
                         [(pair? obj)
                          (+ (r (cdr obj)) 1)]
                         [else (return #f)]))])
         (r obj))))))
[(list-length '(1 2 3 4))                       . -> .  4]
[(list-length '(a b . c))                       . -> .  #f]

[(call-with-current-continuation procedure?)    . -> .  #t]

[(call-with-values
      (lambda () (values 'bond 'james))
    (lambda (x y) (cons y x)))                  . -> . (james . bond)]

[(call-with-values values list)                 . -> . ()]

[(letrec ([split (lambda (ls)
                   (if (or (null? ls) (null? (cdr ls)))
                       (values ls '())
                       (call-with-values
                           (lambda () (split (cddr ls)))
                         (lambda (odds evens)
                           (values (cons (car ls) odds)
                                   (cons (cadr ls) evens))))))])

   (call-with-values (lambda () (split '(a b c d e f)))
     (lambda x x))) . -> . ((a c e) (b d f))]

[(+ (values 2) 4) . -> . 6]

[(if (values #t) 1 2) . -> . 1]

[(call-with-values (lambda () 4)
   (lambda (x) x)) . -> . 4]

[(call-with-values (lambda () (values 4 5))
   (lambda (a b) b))           . -> .  5]
[(call-with-values * -)                        . -> .  -1]

[(let ([path '()]
       [c #f])
   (let ([add (lambda (s)
                (set! path (cons s path)))])
     (dynamic-wind
         (lambda ()
           (add 'connect))
         (lambda ()
           (add (call-with-current-continuation
                 (lambda (c0)
                   (set! c c0)
                   'talk1))))
         (lambda ()
           (add 'disconnect)))
     (if (< (length path) 4)
         (c 'talk2)
         (reverse path))))                     . -> .  (connect talk1 disconnect connect talk2 disconnect)]

[(let ([n 0])
   (call-with-current-continuation
    (lambda (k)
      (dynamic-wind
          (lambda ()
            (set! n (+ n 1))
            (k))
          (lambda ()
            (set! n (+ n 2)))
          (lambda ()
            (set! n (+ n 4))))))
   n)                                          . -> .  1]

[(let ([n 0])
   (call-with-current-continuation
    (lambda (k)
      (dynamic-wind
          values
          (lambda ()
            (dynamic-wind
                values
                (lambda ()
                  (set! n (+ n 1))
                  (k))
                (lambda ()
                  (set! n (+ n 2))
                  (k))))
          (lambda ()
            (set! n (+ n 4))))))
   n)                                          . -> .  7]

;;;; 16. Iteration

[(let loop ([numbers '(3 -2 1 6 -5)]
            [nonneg '()]
            [neg '()])
   (cond [(null? numbers) (list nonneg neg)]
         [(>= (car numbers) 0)
          (loop (cdr numbers)
                (cons (car numbers) nonneg)
                neg)]
         [(< (car numbers) 0)
          (loop (cdr numbers)
                nonneg
                (cons (car numbers) neg))]))   . -> .  ((6 1 3) (-5 -2))]

;;; 17. Quasiquotation

[`(list ,(+ 1 2) 4)                            . -> .  (list 3 4)]
[(let ([name 'a]) `(list ,name ',name))        . -> .  (list a (quote a))]
[`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)         . -> .  (a 3 4 5 6 b)]
[`((foo ,(- 10 3))
   ,@(cdr '(c)) . ,(car '(cons)))              . -> .  ((foo 7) . cons)]
[`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)     . -> .  #(10 5 2 4 3 8)]
;; [(let ((name 'foo))
;;    `((unquote name name name)))                . -> .  (foo foo foo)]
;; [(let ((name '(foo)))
;;    `((unquote-splicing name name name)))       . -> .  (foo foo foo)]
[(let ([q '((append x y) (sqrt 9))])
   ``(foo ,,@q))                               . -> .  `(foo (unquote (append x y) (sqrt 9)))]
;; [(let ((x '(2 3))
;;        (y '(4 5)))
;;    `(foo (unquote (append x y) (sqrt 9))))     . -> .  (foo (2 3 4 5) 3)]

[`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)      . -> .  (a `(b ,(+ 1 2) ,(foo 4 d) e) f)]
[(let ([name1 'x]
       [name2 'y])
   `(a `(b ,,name1 ,',name2 d) e))             . -> .  (a `(b ,x ,'y d) e)]

;;; 18. Binding constructs for syntatic keywords

[(let-syntax ([when2 (syntax-rules ()
                       [(when2 test stmt1 stmt2 ...)
                        (when test
                          stmt1 stmt2 ...)])])
   (let ([if #t])
     (when2 if (set! if 'now))
     if))                                      . -> .  now]

[(let ([x 'outer])
   (let-syntax ([m (syntax-rules () ((m) x))])
     (let ((x 'inner))
       (m))))                                  . -> .  outer]

;; [(let ()
;;    (let-syntax
;;        ((def (syntax-rules ()
;;                ((def stuff ...) (define stuff ...)))))
;;      (def foo 42))
;;    foo)                                        . -> .  42]

;; [(let ()
;;    (let-syntax ())
;;    5)                                          . -> .  5]

[(letrec-syntax
     ([my-or (syntax-rules ()
               [(my-or) #f]
               [(my-or e) e]
               [(my-or e1 e2 ...)
                (let ([temp e1])
                  (if temp
                      temp
                      (my-or e2 ...)))])])
   (let ([x #f]
         [y 7]
         [temp 8]
         [let odd?]
         [if even?])
     (my-or x
            (let temp)
            (if y)
            y)))                               . -> .  7]

[(let ([f (lambda (x) (+ x 1))])
   (let-syntax ([f (syntax-rules ()
                     [(f x) x])]
                (g (syntax-rules ()
                     [(g x) (f x)])))
     (list (f 1) (g 1))))                      . -> .  (1 2)]

[(let ([f (lambda (x) (+ x 1))])
   (letrec-syntax ([f (syntax-rules ()
                        [(f x) x])]
                   [g (syntax-rules ()
                        [(g x) (f x)])])
     (list (f 1) (g 1))))                      . -> .  (1 1)]


(define-syntax be-like-begin
  (syntax-rules ()
    [(be-like-begin name)
     (define-syntax name
       (syntax-rules ()
         [(name expr (... ...))
          (begin expr (... ...))]))]))
(be-like-begin sequence)
[(sequence 1 2 3 4)                             . -> .  4]

[(let ([=> #f])
   (cond [#t => 'ok]))                          . -> .  ok]

;; (define p (cons 4 5))
;; (define-syntax p.car (identifier-syntax (car p)))
;; [p.car                                         . -> .  4]
;; [(set! p.car 15)                               . -> .  &syntax]

;; (define q (cons 4 5))
;; (define-syntax q.car
;;   (identifier-syntax
;;    (_ (car q))
;;    ((set! _ e) (set-car! q e))))
;; (set! p.car 15)
;; [q.car                                         . -> .  15]
;; [q                                             . -> .  (15 5)]
