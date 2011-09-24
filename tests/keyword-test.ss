#lang planet masm/sines

(require
 (rename-in "../test.ss"
            [check-expect ->e]
            [check-expect-one-of ->e/one-of]
            [check-quoted-expect ->]
            [check-quoted-expect-one-of ->/one-of]))

(let ()
  (define/kw (test)
    42)

  [(test)           . -> . 42])

(let ()
  (define/kw (test x)
    x)

  [(test 43)        . -> . 43])



(let ()
  (define/kw (test #:x x)
    x)

  ;;[(test #:x 44)    . -> . 44]
  (display (test #:x 44)) (newline))

(let ()
  (define/kw (test x #:y y)
    (list x y))

  (display (test 45 #:y 46)) (newline)
  (display (test #:y 48 47)) (newline))

(let ()
  (define/kw (test #:x [x 49])
    x)

  (display (test)) (newline)
  (display (test #:x 50)) (newline))

(let ()
  (define/kw (test a #:b b #:c [c 666] d [e 43] . f)
    (list a b c d e f))

  (display (test #:c 42 40 #:b 41 43 44 45)) (newline)
  (display (test 39 #:c 41 #:b 40 42)) (newline)
  (display (test 39 #:b 40 42)) (newline))
