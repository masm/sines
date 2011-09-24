#lang s-exp "../base-lang.rkt"

(require
 "../es/bridge.ss"
 "../lib/regexp.ss")

;; (define-invoker (es-string-char-at string index)                        "charAt")

(define-invoker (_string-index string search-string [position]) "indexOf")

;; (define-invoker (es-string-last-index-of string search-string position) "lastIndexOf")
;; (define-invoker (es-string-slice string start end) "slice")
;; (define-invoker (es-string-downcase string)        "toLowerCase")
;; (define-invoker (es-string-locale-downcase string) "toLocaleLowerCase")
;; (define-invoker (es-string-upcase string)          "toUpperCase")
;; (define-invoker (es-string-locale-upcase string)   "toLocaleUpperCase")

(define string-index
  (case-lambda
    [(str char)
     (let ([x (_string-index str (string char))])
       (and (not (negative? x)) x))]
    [(str char left)
     (let ([x (_string-index str (string char) left)])
       (and (not (negative? x)) x))]
    [(str char left right)
     (let ([x (_string-index str (string char) left)])
       (and (not (negative? x))
            (not (<= right x))
            x))]))

(provide
 string-index)
