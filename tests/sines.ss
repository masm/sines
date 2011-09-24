#lang planet masm/sines

(require
 (rename-in "../test.ss"
            [check-expect ->e]
            [check-expect-one-of ->e/one-of]
            [check-quoted-expect ->]
            [check-quoted-expect-one-of ->/one-of])
 )

;;; 2. Definitions

;;;; 2.1. Variable definitions

;;;; 2.2. Syntax definitions

;;; 3. Bodies

[(let () (define x 1) x)                      . -> . 1]
[((lambda (x) (define y x) y) 1)              . -> . 1]

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

[((lambda () 1))                               . -> . 1]
[((lambda (x) x) 1)                            . -> . 1]
[((lambda (f x y) (f x y)) + 1 2)              . -> . 3]

;;;; 4.3. Conditionals

[(let ((l '(a b c)))
   (if (null? l) '() (cdr l)))                 . -> . (b c)]

[(let ((l '()))
   (if (null? l) '() (cdr l)))                 . -> . ()]

[(let ((abs
        (lambda (x)
          (if (< x 0)
              (- 0 x)
              x))))
   (abs -4))                                   . -> . 4]

[(let ((x -4))
   (if (< x 0)
       (list 'minus (- 0 x))
       (list 'plus 4)))                        . -> . (minus 4)]

;;;; 4.4. Assignments

;;;; 4.5. Derived conditionals

[(let ((x 0))
   (cond
    ((< x 0) (list 'minus (abs x)))
    ((> x 0) (list 'plus x))
    (else (list 'zero x))))                    . -> . (zero 0)]

(let ((select (lambda (x)
                (cond
                 ((not (symbol? x)))
                 ((assq x '((a . 1) (b . 2) (c . 3)))
                  => cdr)
                 (else 0)))))
  [(select 3)                                  . -> . #t]
  [(select 'b)                                 . -> . 2]
  [(select 'e)                                 . -> . 0])

[(let ((x 4) (y 5))
   (case (+ x y)
     ((1 3 5 7 9) 'odd)
     ((0 2 4 6 8) 'even)
     (else 'out-of-range)))                    . -> . odd]

[(or)                                          . -> .  #f]
[(or 1)                                        . -> .  1]
[(or #f 2)                                     . -> .  2]

;;;; 4.6. Binding constructs

[(let () 1)                                    . -> .  1]
[(let ((x 1)) x)                               . -> .  1]
[(let ((f +) (x 1) (y 2)) (f x y))             . -> .  3]

;;;; 4.7. Sequencing

[(begin 1)                                     . -> .  1]
[(begin 1 2)                                   . -> .  2]
[(begin (values) (values 1 2 3) 'a)            . -> . a]

;;; 5. Equivalence predicates

[(eqv? 1 1)                                    . -> . #t]
[(eqv? 1.1 1.1)                                . -> . #t]
[(eqv? 0 1)                                    . -> . #f]
[(eqv? 0.1 1.1)                                . -> . #f]
[(eqv? (list 1) (list 1))                      . -> . #f]
[(eqv? (list (quote a) (quote (b)) (quote c))
       (list (quote a) (quote (b)) (quote c))) . -> . #f]
;; [(eqv? (string #\f) (string #\f)) . -> . #f]
[(eqv? (make-vector 5 (quote a)) (quote #(a a a a a))) . -> . #f]

[(eq? (list 1) (list 1)) . -> . #f]
[(eq? (list (quote a) (quote (b)) (quote c))
      (list (quote a) (quote (b)) (quote c)))  . -> . #f]
;; [(eq? (string #\f) (string #\f)) . -> . #f]
[(eq? (make-vector 5 (quote a))
      (quote #(a a a a a)))                    . -> . #f]

[(equal? #t #t)                               . -> .  #t]
[(equal? #f #f)                               . -> .  #t]
[(equal? #t #f)                               . -> .  #f]

[(equal? 'foo 'foo)                           . -> .  #t]
[(equal? 'foo 'bar)                           . -> .  #f]
[(equal? 'foo #t)                             . -> .  #f]
[(equal? 'foo #f)                             . -> .  #f]

[(equal? 1 #t)                                . -> .  #f]
[(equal? 0 #f)                                . -> .  #f]
[(equal? 65 'a)                               . -> .  #f]

[(equal? #\a #\a)                             . -> .  #t]
[(equal? #\a #\b)                             . -> .  #f]
[(equal? #\a #\A)                             . -> .  #f]
[(equal? #\f #f)                              . -> .  #f]
[(equal? #\t #t)                              . -> .  #f]
[(equal? #\a 'a)                              . -> .  #f]
[(equal? #\a 'A)                              . -> .  #f]
[(equal? #\a 65)                              . -> .  #f]
[(equal? #\a 97)                              . -> .  #f]

[(equal? '() '())                             . -> .  #t]
[(equal? '() #f)                              . -> .  #f]
[(equal? '() #t)                              . -> .  #f]
[(equal? '() 'a)                              . -> .  #f]
[(equal? '() 0)                               . -> .  #f]
[(equal? '() 'nil)                            . -> .  #f]

[(equal? '(()) '())                           . -> .  #f]
[(equal? '(a) 'a)                             . -> .  #f]
[(equal? '(()) '((())))                       . -> .  #f]
[(let ([x '(())]) (equal? x x))               . -> .  #t]

[(equal? "f" #f)                              . -> .  #f]
[(equal? "F" #f)                              . -> .  #f]
[(equal? "t" #t)                              . -> .  #f]
[(equal? "T" #t)                              . -> .  #f]
[(equal? "foo" 'foo)                          . -> .  #f]
[(equal? "1" 1)                               . -> .  #f]
[(equal? "f" #\f)                             . -> .  #f]
[(equal? "" '())                              . -> .  #f]
[(equal? "foo" '(#\f #\o #\o))                . -> .  #f]
[(let ([x "foo"]) (equal? x x))               . -> .  #t]

[(equal? '#() #f)                             . -> .  #f]
[(equal? '#() #t)                             . -> .  #f]
[(equal? '#() 0)                              . -> .  #f]
[(equal? '#() '())                            . -> .  #f]
[(equal? '#() "")                             . -> .  #f]
[(let ([x '#(1 2 3)]) (equal? x x))           . -> .  #t]

[(equal? (lambda () 1) (lambda () 2))         . -> .  #f]
[(let ((x (lambda (x) x))) (equal? x x))      . -> .  #t]
(let ((gen-counter (lambda ()
                     (let ((n 0))
                       (lambda ()
                         (set! n (+ n 1)) n)))))
  [(equal? (gen-counter) (gen-counter))       . -> .  #f])

[(equal? 1 1)                                  . -> .  #t]
[(equal? 1.1 1.1)                              . -> .  #t]
[(equal? 0 1)                                  . -> .  #f]
[(equal? 0.1 1.1)                              . -> .  #f]
[(equal? (list 1) (list 1))                    . -> .  #t]
[(equal? (list 'a '(b) 'c) (list 'a '(b) 'c))  . -> .  #t]
[(equal? (string #\f) (string #\f))            . -> .  #t]
[(equal? (make-vector 5 'a) #(a a a a a))      . -> .  #t]

;;; 6. Procedure predicate

[(procedure? (lambda () 1)) . -> . #t]
[(procedure? (lambda (x) x)) . -> . #t]
[(procedure? (lambda x x)) . -> . #t]
[(procedure? 1) . -> . #f]
[(procedure? 'a) . -> . #f]
[(procedure? 'nil) . -> . #f]
[(procedure? '()) . -> . #f]
[(procedure? '(a . b)) . -> . #f]
[(procedure? '(a b c)) . -> . #f]
[(procedure? #\0) . -> . #f]
[(procedure? #()) . -> . #f]
[(procedure? #(1)) . -> . #f]
[(procedure? "foo") . -> . #f]
[(procedure? (make-string 3 #\a)) . -> . #f]

;;; 7. Arithmetic

;;; 8. Booleans

[(not #t)                                      . -> .  #f]
[(not #\0)                                     . -> .  #f]
[(not (lambda () 1))                           . -> .  #f]

[(boolean? #t)                                 . -> . #t]
[(boolean? #f)                                 . -> . #t]
[(boolean? '#t)                                . -> . #t]
[(boolean? 1)                                  . -> . #f]
[(boolean? 'a)                                 . -> . #f]
[(boolean? '())                                . -> . #f]
[(boolean? '(a))                               . -> . #f]
[(boolean? 'nil)                               . -> . #f]
[(boolean? '#())                               . -> . #f]
[(boolean? #\0)                                . -> . #f]
[(boolean? (lambda () 1))                      . -> . #f]

;;; 9. Pairs and lists

[(pair? (cons 1 2))                            . -> . #t]
[(pair? '(a))                                  . -> . #t]
[(pair? '(a . b))                              . -> . #t]
[(pair? '(a b c))                              . -> . #t]
[(pair? 1)                                     . -> . #f]
[(pair? 'a)                                    . -> . #f]
[(pair? 'nil)                                  . -> . #f]
[(pair? '())                                   . -> . #f]
[(pair? '#(a b))                               . -> . #f]
[(pair? #\0)                                   . -> . #f]
[(pair? (lambda () 1))                         . -> . #f]

[(cons 'a '())                                 . -> . (a)]
[(cons '(a) '(b c))                            . -> . ((a) b c)]
[(cons "a" '(b c))                             . -> . ("a" b c)]
[(cons 'a 'b)                                  . -> . (a . b)]

[(car '(a))                                    . -> . a]
[(car '(a b c))                                . -> . a]
[(car '((a) b c))                              . -> . (a)]

[(cdr '(a))                                    . -> . ()]
[(cdr '(a b c))                                . -> . (b c)]
[(cdr '((a) b c))                              . -> . (b c)]

(let ((x '((((a . b) c . d) (e . f) g . h) ((i . j) k . l) (m . n) o . p)))
  [(car x)                                     . -> .  (((a . b) c . d) (e . f) g . h)]
  [(cdr x)                                     . -> .  (((i . j) k . l) (m . n) o . p)]
  [(caar x)                                    . -> .  ((a . b) c . d)]
  [(cdar x)                                    . -> .  ((e . f) g . h)]
  [(cadr x)                                    . -> .  ((i . j) k . l)]
  [(cddr x)                                    . -> .  ((m . n) o . p)]
  [(caaar x)                                   . -> .  (a . b)]
  [(cdaar x)                                   . -> .  (c . d)]
  [(cadar x)                                   . -> .  (e . f)]
  [(cddar x)                                   . -> .  (g . h)]
  [(caadr x)                                   . -> .  (i . j)]
  [(cdadr x)                                   . -> .  (k . l)]
  [(caddr x)                                   . -> .  (m . n)]
  [(cdddr x)                                   . -> .  (o . p)]
  [(caaaar x)                                  . -> .  a]
  [(cdaaar x)                                  . -> .  b]
  [(cadaar x)                                  . -> .  c]
  [(cddaar x)                                  . -> .  d]
  [(caadar x)                                  . -> .  e]
  [(cdadar x)                                  . -> .  f]
  [(caddar x)                                  . -> .  g]
  [(cdddar x)                                  . -> .  h]
  [(caaadr x)                                  . -> .  i]
  [(cdaadr x)                                  . -> .  j]
  [(cadadr x)                                  . -> .  k]
  [(cddadr x)                                  . -> .  l]
  [(caaddr x)                                  . -> .  m]
  [(cdaddr x)                                  . -> .  n]
  [(cadddr x)                                  . -> .  o]
  [(cddddr x)                                  . -> .  p])

[(null? '())                                   . -> .  #t]
[(null? (list))                                . -> .  #t]
[(null? 1)                                     . -> .  #f]
[(null? 'a)                                    . -> .  #f]
[(null? 'nil)                                  . -> .  #f]
[(null? '(a))                                  . -> .  #f]
[(null? '#(a b))                               . -> .  #f]
[(null? #\0)                                   . -> .  #f]
[(null? (lambda () 1))                         . -> .  #f]

[(list? (list))                                . -> .  #t]
[(list? '(a))                                  . -> .  #t]
[(list? '(a b))                                . -> .  #t]
[(list? '((a) (b)))                            . -> .  #t]
[(list? 1)                                     . -> .  #f]
[(list? 'a)                                    . -> .  #f]
[(list? 'nil)                                  . -> .  #f]
[(list? '#(a b))                               . -> .  #f]
[(list? #\0)                                   . -> .  #f]
[(list? (lambda () 1))                         . -> .  #f]

[(list-tail '(a . b) 1)                        . -> .  b]
[(list-tail 'a 0)                              . -> .  a]

[(list-ref '(1 2 3) 0)                         . -> .  1]
[(list-ref '(1 2 3) 1)                         . -> .  2]
[(list-ref '(1 2 3) 2)                         . -> .  3]

[(map (lambda (x) (+ x 1)) '(0 1 2))           . -> .  (1 2 3)]
[(map + '(1 2 3) '(2 3 4 5) '(1 2 3 4 0 0))    . -> .  (4 7 10)]
[(map + '(1 2 3) '() '(1 2 3))                 . -> .  ()]
[(map + '(1 2 3) '(4 5 6))                     . -> .  (5 7 9)]

[(let ((l '()))
   (for-each (lambda (x) (set! l (cons (+ x 1) l)))
             '(0 1 2))
   l)                                          . -> .  (3 2 1)]

[(let ((l '()))
   (for-each (lambda (x y z)
               (set! l (cons (+ x y z) l)))
             '(1 2 3) '(2 3 4 5 6) '(0 0 0 1 2 3 4))
   l)                                          . -> .  (7 5 3)]

[(let ((l '()))
   (for-each (lambda (x y z)
               (set! l (cons (+ x y z) l)))
             '(1 2 3) '() '(1 2 3))
   l)                                          . -> .  ()]

[(let ((same-count 0))
   (for-each
    (lambda (x y)
      (when (= x y)
        (set! same-count (+ same-count 1))))
    '(1 2 3 4 5 6)
    '(2 3 3 4 7 6))
   same-count)                                 . -> . 3]

;;; 10. Symbols

[(symbol? 'nil)                                 . -> . #t]
[(symbol? 't)                                  . -> . #t]
[(symbol? 'foo)               . -> . #t]
[(symbol? "bar")                . -> . #f]
[(symbol? 1)                  . -> . #f]
[(symbol? '())                . -> . #f]
[(symbol? #t)                 . -> . #f]
[(symbol? #f)                 . -> . #f]

[(symbol->string (string->symbol "FOO"))       . -> . "FOO"]
[(symbol->string (string->symbol "foo"))       . -> . "foo"]
[(string->symbol (symbol->string 'bar))        . -> . bar]
[(symbol->string (string->symbol "a. A:"))     . -> . "a. A:"]

;;; 11. Characters

[(char? #\a) . -> . #t]
[(char? #\0) . -> . #t]
[(char? #\Space) . -> . #t]
[(char? "1") . -> . #f]
[(char? 32) . -> . #f]
[(char? "bar") . -> . #f]
[(char? '#(1)) . -> . #f]
[(char? '#(#\a)) . -> . #f]
[(char? '(#\a)) . -> . #f]
[(char? 'a) . -> . #f]
[(char? '()) . -> . #f]
[(char? #f) . -> . #f]
[(char? #t) . -> . #f]

[(char->integer #\a) . -> . 97]
[(char->integer #\A) . -> . 65]
[(char->integer #\space) . -> . 32]
[(char->integer #\1) . -> . 49]
[(char->integer #\*) . -> . 42]

[(integer->char 97) . -> . #\a]
[(integer->char 65) . -> . #\A]
[(integer->char 32) . -> . #\space]
[(integer->char 49) . -> . #\1]
[(integer->char 42) . -> . #\*]

[(char=?  #\a #\a) . -> . #t]
[(char=? #\a #\A) . -> . #f]

[(char<? #\a #\b) . -> . #t]
[(char<? #\A #\B) . -> . #t]
[(char<? #\1 #\2) . -> . #t]
[(char<? #\a #\a) . -> . #f]
[(char<? #\b #\a) . -> . #f]
[(char<? #\B #\A) . -> . #f]
[(char<? #\2 #\1) . -> . #f]

[(char>? #\b #\a) . -> . #t]
[(char>? #\B #\A) . -> . #t]
[(char>? #\2 #\1) . -> . #t]
[(char>? #\a #\a) . -> . #f]
[(char>? #\a #\b) . -> . #f]
[(char>? #\A #\B) . -> . #f]
[(char>? #\1 #\2) . -> . #f]

[(char<=? #\a #\b) . -> . #t]
[(char<=? #\A #\B) . -> . #t]
[(char<=? #\1 #\2) . -> . #t]
[(char<=? #\a #\a) . -> . #t]
[(char<=? #\b #\a) . -> . #f]
[(char<=? #\B #\A) . -> . #f]
[(char<=? #\2 #\1) . -> . #f]

[(char>=? #\d #\c) . -> . #t]
[(char>=? #\D #\C) . -> . #t]
[(char>=? #\4 #\3) . -> . #t]
[(char>=? #\a #\a) . -> . #t]
[(char>=? #\a #\b) . -> . #f]
[(char>=? #\A #\B) . -> . #f]
[(char>=? #\1 #\2) . -> . #f]

;;; 12. Strings

[(string? "") . -> . #t]
[(string? "foo") . -> . #t]
[(string? #\1) . -> . #f]
[(string? #\a) . -> . #f]
[(string? 32) . -> . #f]
[(string? '#(1)) . -> . #f]
[(string? '#(#\a)) . -> . #f]
[(string? '(#\a)) . -> . #f]
[(string? 'a) . -> . #f]
[(string? '()) . -> . #f]
[(string? #f) . -> . #f]
[(string? #t) . -> . #f]

[(string? (string))                            . -> .  #t]
[(string? (string #\a))                        . -> .  #t]
[(string? (string #\a #\a))                    . -> .  #t]
[(string=? "" (string))                        . -> .  #t]
[(string=? "x" (string #\x))                   . -> .  #t]
[(string=? "xy" (string #\x #\y))              . -> .  #t]
[(string=? "abcde"
           (string #\a #\b #\c #\d #\e))       . -> .  #t]

[(string=? "" "")                              . -> .  #t]
[(string=? "a" "a")                            . -> .  #t]
[(string=? "abc" "abc")                        . -> .  #t]
[(string=? "" (make-string 0))                 . -> .  #t]
[(string=? "" (make-string 0 #\a))             . -> .  #t]
[(string=? "a" (make-string 1 #\a))            . -> .  #t]
[(string=? "aaa" (make-string 3 #\a))          . -> .  #t]
[(string=? "a" "A")                            . -> .  #f]
[(string=? "aa" "aA")                          . -> .  #f]
[(string=? "" "a")                             . -> .  #f]
[(string=? "a" "")                             . -> .  #f]
[(string=? "a" "b")                            . -> .  #f]
[(string=? "a" "A")                            . -> .  #f]

[(string? (make-string 0)) . -> . #t]
[(string? (make-string 3)) . -> . #t]
[(string? (make-string 3 #\x)) . -> . #t]
[(make-string 0) . -> . ""]
[(make-string 0 #\x) . -> . ""]
[(string-length (make-string 100)) . -> . 100]
[(string-length (make-string 100 #\x)) . -> . 100]
[(make-string 3 #\x) . -> . "xxx"]

(define-syntax (check-order stx)
  (syntax-case stx ()
    [(_ pred first more ... last)
     (let ([l (syntax->list #'(more ...))])
       (with-syntax ([(a ...) (cons #'first l)]
                     [(b ...) (append l (list #'last))])
         #'(begin
             [(pred a b) . -> . #t] ...)))]))

(define-syntax (check-order/reverse stx)
  (syntax-case stx ()
    [(_ pred first more ... last)
     (let ([l (syntax->list #'(more ...))])
       (with-syntax ([(a ...) (cons #'first l)]
                     [(b ...) (append l (list #'last))])
         #'(begin
             [(pred b a) . -> . #t] ...)))]))

(check-order string<? ""
             " " "  " " 1" " A" " a"
             "1" "1 " "11" "1A" "1a"
             "A" "A " "A1" "AA" "Aa"
             "a" "a " "a1" "aA" "aa")
(check-order/reverse string>? ""
                     " " "  " " 1" " A" " a"
                     "1" "1 " "11" "1A" "1a"
                     "A" "A " "A1" "AA" "Aa"
                     "a" "a " "a1" "aA" "aa")
(check-order string<=? ""
             " " "  " " 1" " A" " a" " a"
             "1" "1 " "11" "1A" "1a" "1a"
             "A" "A " "A1" "AA" "Aa" "Aa"
             "a" "a " "a1" "aA" "aa" "aa")
(check-order/reverse string>=? ""
                     " " "  " " 1" " A" " a" " a"
                     "1" "1 " "11" "1A" "1a" "1a"
                     "A" "A " "A1" "AA" "Aa" "Aa"
                     "a" "a " "a1" "aA" "aa" "aa")

[(string-length "") . -> . 0]
[(string-length "x") . -> . 1]
[(string-length "asdf") . -> . 4]
[(string-length (string #\a #\b #\c #\d #\e)) . -> . 5]
[(string-length (make-string 10)) . -> . 10]
[(string-length (make-string 10 #\x)) . -> . 10]

[(string-ref "1" 0) . -> . #\1]
[(string-ref "123" 0) . -> . #\1]
[(string-ref "123" 1) . -> . #\2]
[(string-ref "123" 2) . -> . #\3]
[(string-ref (string #\1 #\2 #\3) 2) . -> . #\3]
[(string-ref (make-string 5 #\x) 0) . -> . #\x]
[(string-ref (make-string 5 #\x) 2) . -> . #\x]
[(string-ref (make-string 5 #\x) 4) . -> . #\x]

[(substring "ola" 0 0)                         . -> .  ""]
[(substring "ola" 2 2)                         . -> .  ""]
[(substring "ola" 0 1)                         . -> .  "o"]
[(substring "ola" 1 2)                         . -> .  "l"]
[(substring "ola" 2 3)                         . -> .  "a"]
[(substring "ola" 0 2)                         . -> .  "ol"]
[(substring "ola" 1 3)                         . -> .  "la"]
[(substring "ola" 0 3)                         . -> .  "ola"]
[(substring (make-string 5 #\a) 2 4)           . -> .  "aa"]

[(string-append "a" "b" "c")                   . -> .  "abc"]
[(string-append "" "a" "" "b" "" "c" "")       . -> .  "abc"]
[(string-append "ab" "c" "" "de")              . -> .  "abcde"]
[(string-append "" "" (make-string 0 #\Space)) . -> .  ""]
[(string-append (make-string 1 #\o)
                "l"
                (string #\a))                  . -> .  "ola"]

[(string->list "")                             . -> .  ()]
[(string->list "a")                            . -> .  (#\a)]
[(string->list "ab")                           . -> .  (#\a #\b)]
[(string->list "a b")                          . -> .  (#\a #\Space #\b)]

[(list->string '())                            . -> .  ""]
[(list->string '(#\a))                         . -> .  "a"]
[(list->string '(#\a #\b))                     . -> .  "ab"]
[(list->string '(#\a #\Space #\b))             . -> .  "a b"]
[(list->string (string->list "abcdef"))        . -> .  "abcdef"]


(let* ((s1 "")
       (s2 (string-copy s1)))
  [(string=? s1 s2)                           . -> . #t]
  ;; [(eq? s1 s2)                                . -> . #f]
  )

(let* ((s1 "abc")
       (s2 (string-copy s1)))
  [(string=? s1 s2)                           . -> . #t]
  ;; [(eq? s1 s2)                                . -> . #f]
  )

(let* ((s1 (make-string 5 #\a))
       (s2 (string-copy s1)))
  [(string=? s1 s2)                           . -> . #t]
  ;; [(eq? s1 s2)                                . -> . #f]
  )

(let* ((s1 (string #\a #\b #\c))
       (s2 (string-copy s1)))
  [(string=? s1 s2)                           . -> . #t]
  ;; [(eq? s1 s2)                                . -> . #f]
  )

;;; 13. Vectors

[(vector? (vector)) . -> . #t]
[(vector? (vector 1)) . -> . #t]
[(vector? (make-vector 5)) . -> . #t]
[(vector? (make-vector 5 #\a)) . -> . #t]
[(vector? 1) . -> . #f]
[(vector? 'a) . -> . #f]
[(vector? 'nil) . -> . #f]
[(vector? '()) . -> . #f]
[(vector? '(a . b)) . -> . #f]
[(vector? '(a b c)) . -> . #f]
[(vector? #\0) . -> . #f]
[(vector? (lambda () 1)) . -> . #f]
[(vector? "foo") . -> . #f]
[(vector? (make-string 3 #\a)) . -> . #f]

[(make-vector 2 'a) . -> . #(a a)]
[(make-vector 0 1) . -> . #()]
[(make-vector 0) . -> . #()]
[(make-vector 1 'x) . -> . #(x)]

[(vector-length (vector)) . -> . 0]
[(vector-length (vector #\a)) . -> . 1]
[(vector-length #(1 2)) . -> . 2]
[(vector-length (make-vector 100 'a)) . -> . 100]

[(vector-ref '#(a) 0) . -> . a]
[(vector-ref #(#(0) #(1)) 1) . -> . #(1)]

(let ((vec (make-vector 5 'a)))
  (vector-set! vec 0 0)
  [(vector-ref vec 0) . -> . 0]
  [(vector-ref vec 1) . -> . a]
  (vector-set! vec 1 1)
  [(vector-ref vec 1) . -> . 1]
  (vector-set! vec 2 2)
  (vector-set! vec 3 3)
  (vector-set! vec 4 4)
  [(vector-ref vec 0) . -> . 0]
  [(vector-ref vec 1) . -> . 1]
  [(vector-ref vec 2) . -> . 2]
  [(vector-ref vec 3) . -> . 3]
  [(vector-ref vec 4) . -> . 4]
  (vector-set! vec 3 'a)
  [(vector-ref vec 3) . -> . a])


[(vector->list #())                            . -> .  ()]
[(vector->list #(1))                           . -> .  (1)]
[(vector->list #(1 2))                         . -> .  (1 2)]
[(vector->list (list->vector '(a b c d e)))    . -> .  (a b c d e)]

[(vector-fill! (make-vector 3) 'a)             . -> .  #(a a a)]

;;; 14. Errors and violations

;;; 15. Control features

[(apply + '())                                 . -> . 0]
[(apply + 1 '())                               . -> . 1]
[(apply + 1  2 '())                            . -> . 3]
[(apply apply + 1  2 '(()))                    . -> . 3]
[(apply + '(4 5))                              . -> . 9]
[(apply min '(6 8 3 2 5))                      . -> . 2]
[(apply min  5 1 3 '(6 8 3 2 5))               . -> . 1]
[(apply vector 'a 'b '(c d e))                 . -> . #(a b c d e)]
[(apply (lambda (x . y) x) '(a b c d))         . -> . a]
[(apply (lambda (x . y) y) '(a b c d))         . -> . (b c d)]

(let ((member (lambda (x ls)
                (call/cc
                 (lambda (break)
                   (do ((ls ls (cdr ls)))
                       ((null? ls) #f)
                     (when (equal? x (car ls))
                       (break ls))))))))
  [(member 'd '(a b c))                        . -> . #f]
  [(member 'b '(a b c))                        . -> . (b c)])

[(call-with-values (lambda () (values))
   (lambda x x))                               . -> . ()]

[(call-with-values (lambda () (values 1))
   (lambda x x))                               . -> . (1)]

[(call-with-values (lambda () (values 1 2 3))
   (lambda x x))                               . -> . (1 2 3)]

[(call-with-values (lambda ()
                     ((lambda (ls)
                        (values (car ls) (cdr ls)))
                      '(a b c)))
   (lambda x x))                               . -> . (a (b c))]

[(begin (values 1 2 3) 4)                      . -> . 4]

[(call-with-values
     (lambda ()
       (call/cc (lambda (k) (k 2 3))))
   (lambda (x y) (list x y)))                  . -> . (2 3)]

(begin
  (define d (delay (values 1 2 3)))
  [(call-with-values (lambda () (force d))
     (lambda x x))                             . -> . (1 2 3)]
  [(call-with-values
       (lambda () (force d))
     +)                                        . -> . 6])

;; (begin ;; Tests that must fail
;;   (if (values 1 2) 'x 'y)
;;   (+ (values) 5)
;;   (call-with-values
;;       (lambda () (values 2 3 4))
;;     (lambda (x y) x))
;;   (call-with-values
;;       (lambda () (call/cc (lambda (k) (k 0))))
;;     (lambda (x y) x))
;;   )

;;;; 16. Iteration

(let ((divisors (lambda (n)
                  (let f ((i 2))
                    (cond ((>= i n) '())
                          ((integer? (/ n i))
                           (cons i (f (+ i 1))))
                          (else (f (+ i 1))))))))
  [(divisors 5) . -> . ()]
  [(divisors 32) . -> . (2 4 8 16)])

(let ((divisors (lambda (n)
                  (let f ((i 2)
                          (ls '()))
                    (cond ((>= i n) (reverse ls))
                          ((integer? (/ n i))
                           (f (+ i 1) (cons i ls)))
                          (else (f (+ i 1) ls)))))))
  [(divisors 5) . -> . ()]
  [(divisors 32) . -> . (2 4 8 16)])

;;; 17. Quasiquotation

;;; 18. Binding constructs for syntatic keywords
