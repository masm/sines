#lang s-exp "bootstrap-lang.rkt"

(require
 "../compiler/primitives.rkt")

(define continuation-index -1)
(define continuation-state (es-array))
(define current-toplevel #f)
(define stack-traces (es-array))
(define tail-object (es-object))

(define store-continuation-exception
  (store-continuation-exception-constructor-procedure))

(define switch-continuation-exception
  (switch-continuation-exception-constructor-procedure))

(define tail-trampoline
  (tail-trampoline-constructor-procedure))

(define continuation-trampoline
  (continuation-trampoline-procedure continuation-index
                                     continuation-state
                                     current-toplevel
                                     stack-traces
                                     store-continuation-exception
                                     switch-continuation-exception))

(define tail-trampoline-restart
  (tail-trampoline-restart-procedure continuation-index
                                     continuation-state
                                     tail-object
                                     store-continuation-exception
                                     switch-continuation-exception
                                     tail-trampoline))

(define arguments->rest-parameter
  (arguments->rest-parameter-procedure))

(provide
 continuation-index
 continuation-state
 current-toplevel
 stack-traces
 tail-object
 store-continuation-exception
 switch-continuation-exception
 tail-trampoline
 continuation-trampoline
 tail-trampoline-restart
 arguments->rest-parameter)
