#lang s-exp "../sines-lang.rkt"

(define-primitive-invoker (_install-timeout proc millis) (es-global "setTimeout"))
(define-primitive-invoker (uninstall-timeout id) (es-global "clearTimeout"))
(define-primitive-invoker (_install-interval proc millis) (es-global "setInterval"))
(define-primitive-invoker (uninstall-interval id) (es-global "clearInterval"))

(define (install-timeout millis proc)
  (_install-timeout (wrap-for-callback proc) millis))

(define (install-interval millis proc)
  (_install-interval (wrap-for-callback proc) millis))

(provide
 install-interval uninstall-interval
 install-timeout  uninstall-timeout
 (rename-out [install-interval on-interval]
             [install-timeout  on-timeout]))
