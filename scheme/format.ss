#lang s-exp "../base-lang.rkt"

(require
 "../lib/regexp.ss"
 "../es/to-string.ss"
 "../es/bridge.ss")

(define (format str . argz)
  (define (s1 strs args)
    (if (null? strs)
        (error 'format "too many arguments" str argz)
        (cons (car strs) (s2 (cdr strs) args))))

  (define (s2 strs args)
    (if (null? args)
        (begin
          (unless (null? strs)
            (error 'format "not enough arguments" str argz))
          '())
        (cons (display-to-string (car args)) (s1 strs (cdr args)))))

  (let ([strs (regexp-split (regexp "~[sSaA]") str)])
    (apply string-append (s1 strs argz))))

(provide format
         display-to-string
         write-to-string)

;;; TOMO

(define-invoker (number->string/exponencial number fraction-digits) "toExponencial")
(define-invoker (number->string/fixed number fraction-digits) "toFixed")
(define-invoker (number->string/precision number precision) "toPrecision")

(provide number->string/exponencial
         number->string/fixed
         number->string/precision)