#lang s-exp "../base-lang.rkt"

(require
 "../es/bridge.ss")

(define-primitive-invoker (decode-uri string) (es-global "decodeURI"))
(define-primitive-invoker (decode-uri-component string) (es-global "decodeURIComponent"))
(define-primitive-invoker (encode-uri string) (es-global "encodeURI"))
(define-primitive-invoker (encode-uri-component string) (es-global "encodeURIComponent"))

(provide decode-uri
         decode-uri-component
         encode-uri
         encode-uri-component)
