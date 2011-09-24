#lang s-exp "../base-lang.rkt"

(require
 "../es/bridge.ss"
 "../lib/regexp.ss")

;;; characters

(define (lookup-table2 table c)
  (let ([i (char->integer c)])
    (if (< 64 i 256)
        (string-ref table (- i 65))
        c)))

(define (char-upcase c)
  (lookup-table2
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~\u007F\u0080\u0081\u0082\u0083\u0084\u0085\u0086\u0087\u0088\u0089\u008A\u008B\u008C\u008D\u008E\u008F\u0090\u0091\u0092\u0093\u0094\u0095\u0096\u0097\u0098\u0099\u009A\u009B\u009C\u009D\u009E\u009F ¡¢£¤¥¦§¨©ª«¬\u00AD®¯°±²³´Μ¶·¸¹º»\u00BC\u00BD\u00BE¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞŸ"
   c))

(define (char-downcase c)
  (lookup-table2
   "abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u007F\u0080\u0081\u0082\u0083\u0084\u0085\u0086\u0087\u0088\u0089\u008A\u008B\u008C\u008D\u008E\u008F\u0090\u0091\u0092\u0093\u0094\u0095\u0096\u0097\u0098\u0099\u009A\u009B\u009C\u009D\u009E\u009F ¡¢£¤¥¦§¨©ª«¬\u00AD®¯°±²³´µ¶·¸¹º»\u00BC\u00BD\u00BE¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"
   c))

(define char-titlecase char-upcase)

(define (char-foldcase c)
  (lookup-table2
   "abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u007F\u0080\u0081\u0082\u0083\u0084\u0085\u0086\u0087\u0088\u0089\u008A\u008B\u008C\u008D\u008E\u008F\u0090\u0091\u0092\u0093\u0094\u0095\u0096\u0097\u0098\u0099\u009A\u009B\u009C\u009D\u009E\u009F ¡¢£¤¥¦§¨©ª«¬\u00AD®¯°±²³´μ¶·¸¹º»\u00BC\u00BD\u00BE¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"
   c))

(provide char-upcase
         char-downcase
         char-titlecase
         char-foldcase)

;; char-ci=?
;; char-ci<?
;; char-ci>?
;; char-ci<=?
;; char-ci>=?

(define (lookup-table table c)
  (let ([i (char->integer c)])
    (and (< i 256)
         (char=? #\1 (string-ref table i)))))

(define (char-alphabetic? c)
  (lookup-table
   "0000000000000000000000000000000000000000000000000000000000000000011111111111111111111111111000000111111111111111111111111110000000000000000000000000000000000000000000000010000000000100001000001111111111111111111111101111111111111111111111111111111011111111"
   c))

(define (char-numeric? c)
  (lookup-table
   "0000000000000000000000000000000000000000000000001111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110000010000000000000000000000000000000000000000000000000000000000000000000000"
   c))

(define (char-whitespace? c)
  (lookup-table
   "0000000001111100000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
   c))

(define (char-upper-case? c)
  (lookup-table
   "0000000000000000000000000000000000000000000000000000000000000000011111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111101111111000000000000000000000000000000000"
   c))

(define (char-lower-case? c)
  (lookup-table
   "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111111111111111111111111110000000000000000000000000000000000000000000000010000000000100001000000000000000000000000000000000000111111111111111111111111011111111"
   c))

(define (char-title-case?)
  #f)

(provide  char-alphabetic?
          char-numeric?
          char-whitespace?
          char-upper-case?
          char-lower-case?
          char-title-case?)

;;; strings

(define (string-upcase str)
  (es-call-property str "toUpperCase"))

(define (string-downcase str)
  (es-call-property str "toLowerCase"))

;; string-titlecase
;; string-foldcase

(provide string-upcase
         string-downcase)


(define (string-ci=? s1 s2)
  (string=? (string-downcase s1) (string-downcase s2)))

(define (string-ci<? s1 s2)
  (string<? (string-downcase s1) (string-downcase s2)))

(define (string-ci>? s1 s2)
  (string>? (string-downcase s1) (string-downcase s2)))

(define (string-ci<=? s1 s2)
  (string<=? (string-downcase s1) (string-downcase s2)))

(define (string-ci>=? s1 s2)
  (string>=? (string-downcase s1) (string-downcase s2)))

(provide string-ci=?
         string-ci<?
         string-ci>?
         string-ci<=?
         string-ci>=?)

;;;

(define-invoker (string-compare string other-string) "localeCompare")

(provide string-compare)

;; (define-invoker (es-string-char-at string index) "charAt")
;; (define-invoker (es-string-index-of string search-string position) "indexOf")
;; (define-invoker (es-string-last-index-of string search-string position) "lastIndexOf")
;; (define-invoker (es-string-slice string start end) "slice")
;; (define-invoker (es-string-downcase string) "toLowerCase")
;; (define-invoker (es-string-locale-downcase string) "toLocaleLowerCase")
;; (define-invoker (es-string-upcase string) "toUpperCase")
;; (define-invoker (es-string-locale-upcase string) "toLocaleUpperCase")

