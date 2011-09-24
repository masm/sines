#lang racket/base

(require
 rackunit
 (prefix-in syntax: "../compiler/syntax.rkt")
 (prefix-in parse: "../compiler/parse.rkt")
 (prefix-in contraction: "../compiler/passes/contraction2.rkt"))

(require/expose "../compiler/passes/contraction2.rkt" [use-count-hashes update-use-count-hashes!])

(define (transform form)
  (parse:unparse (contraction:transform (parse:parse form))))

(define-syntax-rule (check-contraction msg form1 form2)
  (check-equal? (transform 'form1) 'form2 msg))

(define-syntax-rule (check-lexicals-use-count form [symbol count] ...)
  (let-values ([(lexical global) (use-count-hashes (parse:parse 'form))])
    (let ([hash (lexical-env->symbol-hash lexical)])
      (check-eqv? (hash-ref hash 'symbol) count)) ...))

(define-syntax-rule (check-update-use-count-hashes form [symbol count] ...)
  (let ([node (parse:parse 'form)])
    (let-values ([(lexical global) (use-count-hashes node)])
      (update-use-count-hashes! lexical global node sub1)
      (let ([hash (lexical-env->symbol-hash lexical)])
        (check-eqv? (hash-ref hash 'symbol) count)) ...)))

(define (lexical-env->symbol-hash lexical-env)
  (let ([hash (make-hasheq)])
    (hash-for-each lexical-env
                   (lambda (key val)
                     (hash-set! hash (syntax:id-name key) val)))
    hash))

;;; use-count-hashes

(test-case
 "lexical use counts"
 (check-lexicals-use-count (fix ([x (lambda () (y))]
                                 [y (lambda () (x))])
                             42)

                           [x 1] [y 1]))


(test-case
 "update use count hashes"
 (check-update-use-count-hashes (fix ([x (lambda () (y))]
                                      [y (lambda () (x))])
                                  42)

                                [x 0] [y 0]))

;;; fix

(test-case
  "fix contraction"
  (check-contraction "Removes fix if no variable is used"
                     (fix ([x (lambda () 1)])
                       1)

                     1)

  (check-contraction "Removes fix if variable is used recursively"
                     (fix ([x (lambda () (x))])
                       1)

                     1)

  (check-contraction "Removes fix if variables are used recursively"
                     (fix ([x (lambda () (y))]
                           [y (lambda () (x))])
                       42)

                     42)

  (check-contraction "Removes unused variable, even if it appears in the body"
                     (fix ([x (lambda () 1)])
                       (if #t 1 (begin (x) (x))))

                     1))

(test-case
  "fix inlining in body"
  (check-contraction "Inlines a simple function"
                     (fix ([x (lambda () 1)])
                       x)

                     (lambda () 1))

  (check-contraction "Inlines a simple function, even if it used in an unused function"
                     (fix ([x (lambda () 1)]
                           [y (lambda () x)])
                       x)

                     (lambda () 1))

  (check-contraction "Inlines use, even if it appears multiple times in the body"
                     (fix ([x (lambda () 1)])
                       (if #t (x) (begin (x) (x))))

                     1)

  (check-contraction "Inlines a simple function, even if it used in an unused function and appears multiple times in the body"
                     (fix ([x (lambda () 1)]
                           [y (lambda () x)])
                       (if #t (x) (x)))

                     1))

(test-case
  "fix inlining in other proc"
  (check-contraction "Simple inline"
                     (fix ([x (lambda () (y))]
                           [y (lambda () 1)])
                       (begin (x) (x)))

                     (fix ([x (lambda () 1)])
                       (begin (x) (x))))

  (check-contraction "propagate inline"
                     (fix ([x (lambda () (y))]
                           [y (lambda () 1)])
                       (x))

                     1)

  (check-contraction "propagate inline, even if it appears in the body"
                     (fix ([x (lambda () (y))]
                           [y (lambda () 1)])
                       (if #t (x) (y)))

                     1))

(test-case
  "fix not inlining"
  (check-contraction "Does not inline a simple function if used more than once in the body"
                     (fix ([x (lambda () 1)])
                       (begin (x) (x)))

                     (fix ([x (lambda () 1)])
                       (begin (x) (x))))

  (check-contraction "Does not inline a recursive function"
                     (fix ([x (lambda () (x))])
                       (x))

                     (fix ([x (lambda () (x))])
                       (x)))

(check-contraction "Does not drop used recursive function, if it appears in the body"
                   (fix ([x (lambda () x)]
                         [y (lambda () y)])
                     (if #t (x) (y)))

                   (fix ([x (lambda () x)]
                         [y (lambda () y)])
                     (x))))

(check-contraction ""
                   (fix ([x (lambda () (x))])
                     (if #t 1 (x)))

                   (fix ([x (lambda () (x))])
                     1))

(check-contraction ""
                   (fix ([x (lambda () (begin (y) (x)))]
                         [y (lambda () (z))]
                         [z (lambda () (x))])
                     (if #t 1 (x)))

                   (fix ([x (lambda () (begin (x) (x)))])
                     1))

(check-contraction ""
                   (fix ([x (lambda () (begin (x) (x)))])
                     1)

                   (fix ([x (lambda () (begin (x) (x)))])
                     1))

(check-contraction ""
                   (fix ([x (lambda () (x))]
                         [y (lambda () (x))])
                     (if #t 1 (y)))

                   (fix ([x (lambda () (x))])
                     1))

(check-contraction ""
                   (fix ([y (lambda () (x))]
                         [x (lambda () (x))])
                     (if #t 1 (y)))

                   (fix ([x (lambda () (x))])
                     1))

;;; let-values

(check-contraction ""
                   (let-values ([() 1])
                     1)

                   1)

(test-begin
 (check-contraction ""
                    (fix ([y (lambda () (y))])
                      (let-values ([(x) (y)])
                        1))

                    (fix ([y (lambda () (y))])
                      (begin (y) 1)))

 (check-contraction ""
                    (fix ([y (lambda () (y))])
                      (let-values ([(x) (begin 1 (y))])
                        1))

                    (fix ([y (lambda () (y))])
                      (begin (y) 1))))