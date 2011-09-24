#lang planet masm/sines

(define (execute-test-pred-with equality-pred thunk e)
  (with-exception-handler
   (lambda (ex)
     (display ex) (newline)
     #f)
   (lambda () (equality-pred (thunk) e))))

(define (check-expect-helper-aux equality-pred test-thunk test-form expected)
  (with-exception-handler
   (lambda (ex)
     (failure "~S thrown an exception ~S; expected ~S" test-form ex expected))
   (lambda ()
     (let ([test-result (test-thunk)])
       (if (equality-pred test-result expected)
           (success test-form expected)
           (failure "~S evaluated to ~S; expected ~S" test-form test-result expected))))))

(define-syntax-rule (check-expect-helper equality-pred test expected)
  (check-expect-helper-aux equality-pred (lambda () test) 'test expected))

(define (check-expect-one-of-helper-aux equality-pred test-thunk test-form . expected-list)
  (with-exception-handler
   (lambda (ex)
     (failure "~S thrown an exception ~S; expected one of ~S" test-form ex expected-list))
   (lambda ()
     (let ([test-result (test-thunk)])
       (cond [(find (lambda (expected)
                      (equality-pred test-result expected))
                    expected-list)
              => (lambda (expected) (success test-form expected))]
             [else (failure "~S evaluated to ~S; expected one of ~S" test-form test-result expected-list)])))))

(define-syntax-rule (check-expect-one-of-helper equality-pred test expected more-expected ...)
  (check-expect-one-of-helper-aux equality-pred (lambda () test) 'test expected more-expected ...))

(define-syntax (check-expect stx)
  (syntax-case stx ()
    [(_  test expected)
     #'(check-expect-helper equal? test expected)]))

(define-syntax (check-expect-one-of stx)
  (syntax-case stx ()
    [(_  test expected more-expected ...)
     #'(check-expect-one-of-helper equal? test expected more-expected ...)]))

(define-syntax (check-expect-eq stx)
  (syntax-case stx ()
    [(_  test expected)
     #'(check-expect-helper eq? test expected)]))

(define-syntax (check-expect-eq-one-of stx)
  (syntax-case stx ()
    [(_  test expected more-expected ...)
     #'(check-expect-one-of-helper eq? test expected more-expected ...)]))

(define-syntax (check-expect-eqv stx)
  (syntax-case stx ()
    [(_  test expected)
     #'(check-expect-helper eqv? test expected)]))

(define-syntax (check-expect-eqv-one-of stx)
  (syntax-case stx ()
    [(_  test expected more-expected ...)
     #'(check-expect-one-of-helper eqv? test expected more-expected ...)]))

(define-syntax (check-quoted-expect stx)
  (syntax-case stx ()
    [(_  test expected)
     #'(check-expect test 'expected)]))

(define-syntax (check-quoted-expect-one-of stx)
  (syntax-case stx ()
    [(_  test expected more-expected ...)
     #'(check-expect-one-of test 'expected 'more-expected ...)]))

(define error-count 0)

(define failure-list '())
(define success-list '())

(define (error-x name message . args)
  (display name) (display ": ") (display message) (display " - ") (display args) (newline))

(define (failure message . args)
  (apply error-x 'check-expect message args)
  (set! error-count (add1 error-count))
  (set! failure-list (cons (cons message args)
                           failure-list)))

(define (success test expected)
  (set! success-list (cons (list '->/v test expected) success-list))
  ;; (display test)
  ;; (newline)
  )

(define (report-test-summary)
  (newline)
  (display (format "\n ~A tests ran\n ~A tests passed\n ~A tests failed\n"
                   (+ (length success-list) (length failure-list))
                   (length success-list)
                   (length failure-list)))
  (newline))

(provide
 check-expect
 check-quoted-expect
 check-expect-one-of
 check-quoted-expect-one-of
 report-test-summary)

;;;

(define-syntax-rule (check-true a)
  (check-expect a #t))

(define-syntax-rule (check-false a)
  (check-expect a #f))

(define-syntax-rule (check-equal? a b)
  (check-expect a b))

(define-syntax-rule (check-eqv? a b)
  (check-expect-eqv a b))

(provide
 check-true
 check-false
 check-equal?
 check-eqv?)

