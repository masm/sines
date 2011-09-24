#lang s-exp "../base-lang.rkt"

(require
 "bridge.ss")

(define (to-string/char c)
  (string-append "#\\" (string c)))

(define (to-string/pair c recur)
  (define (fn l)
    (cond [(null? (cdr l)) (recur (car l))]
          [(pair? (cdr l)) (string-append (recur (car l)) " " (fn (cdr l)))]
          [else            (string-append (recur (car l)) " . " (recur (cdr l)))]))
  (string-append "(" (fn c) ")"))

(define (to-string/vector v recur)
  (let ([l (vector->list v)])
    (if (null? l) "#()" (string-append "#" (to-string/pair l recur)))))

(define (display-to-string obj)
  (cond [(null? obj)         "()"]
        [(es-undefined? obj) "#<undefined>"]
        [(symbol? obj)       (symbol->string obj)]
        [(pair? obj)         (to-string/pair obj display-to-string)]
        [(vector? obj)       (to-string/vector obj display-to-string)]
        [(char? obj)         (string obj)]
        [(boolean? obj)      (if obj "#t" "#f")]
        [(procedure? obj)    "#<procedure>"]
        [else                (es-call-property obj "toString")]))

(define (write-to-string obj)
  (cond [(null? obj)         "()"]
        [(es-undefined? obj) "#<undefined>"]
        [(symbol? obj)       (symbol->string obj)]
        [(string? obj)       (string-append "\"" obj "\"")]
        [(pair? obj)         (to-string/pair obj write-to-string)]
        [(vector? obj)       (to-string/vector obj write-to-string)]
        [(char? obj)         (to-string/char obj)]
        [(boolean? obj)      (if obj "#t" "#f")]
        [(procedure? obj)    "#<procedure>"]
        [else                (es-call-property obj "toString")]))

(provide ;; to-string/char to-string/pair to-string/vector
         display-to-string
         write-to-string)
