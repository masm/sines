#lang s-exp "../base-lang.rkt"

(require
 "../es/bridge.ss"
 "sset.ss")

;;; RexExp objects

;; TODO: use this
;; (define-invoker (es-string-search string regexp) "search")
;; (define-invoker (es-regexp-exec regexp string) "exec")
;; (define/get+set (es-regexp->last-index regexp) "lastIndex")

;;; String object

(define (regexp? obj)
  (%%regexp? obj))

(define (regexp str)
  (%%regexp str))

(define (regexp/i str)
  (%%regexp str "i"))

(define (regexp/m str)
  (%%regexp str "m"))

(define (regexp/im str)
  (%%regexp str "im"))

(provide regexp?
         regexp regexp/i regexp/im regexp/m)

;;;

(define regexp-match
  (case-lambda
    [(pattern input)            (regexp-match2 pattern input)]
    [(pattern input left)       (regexp-match2 pattern (substring input left))]
    [(pattern input left right) (regexp-match2 pattern (substring input left right))]))

(define (regexp-match2 pattern input)
  (let ([r (%%regexp-match pattern input)])
    (if (null? r) #f (vector->list r))))

(define regexp-match?
  (case-lambda
    [(pattern input)            (%%regexp-match? pattern input)]
    [(pattern input left)       (%%regexp-match? pattern (substring input left))]
    [(pattern input left right) (%%regexp-match? pattern (substring input left right))]))

(define regexp-split
  (case-lambda
    [(pattern input)            (regexp-split-aux pattern input)]
    [(pattern input left)       (regexp-split-aux pattern (substring input left))]
    [(pattern input left right) (regexp-split-aux pattern (substring input left right))]))

(define (regexp-split-aux pattern input)
  (let ([r (%%regexp-split pattern input -1)])
    (if (null? r) #f (vector->list r))))

(define (regexp-replace pattern input insert)
  (%%regexp-replace pattern input insert))

(define (regexp-replace* pattern input insert)
  (%%regexp-replace (%%global-regexp pattern) input insert))

(provide regexp-match
         regexp-match?
         regexp-split
         regexp-replace
         regexp-replace*)

;;;

(define special-regexp-chars
  (sset "^" "$" "\\" "." "*" "+" "?" "(" ")" "[" "]" "{" "}" "|"))

(define (special-regexp-char? char)
  (sset-member? special-regexp-chars (string char)))

(define (escaped-regexp-string str)
  (regexp-replace* (regexp "[$^\\\\.*+?()[\\]{}|]") str "\\$&"))

(provide special-regexp-char?
         escaped-regexp-string)