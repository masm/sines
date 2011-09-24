#lang scheme/base

(require (prefix-in b: "../backend.rkt"))

(define-struct backend
  (js-version
   has-array-comprehension?
   has-let?
   wait-load?
   header))

(define current-backend
  (make-parameter
   (case (string->symbol b:backend)
     [(chrome)        (make-backend 170 #f #f #t #f)]
     [(gecko)         (make-backend 170 #f #t #t #f)]
     [(gecko-ext)     (make-backend 170 #f #t #f #f)]
     [(gecko-worker)  (make-backend 170 #f #t #f #f)]
     [(gecko2)        (make-backend 185 #f #t #t #f)]
     [(gecko2-ext)    (make-backend 185 #f #t #f #f)]
     [(gecko2-worker) (make-backend 185 #f #t #f #f)]
     [(node)          (make-backend 170 #f #f #f "#!/usr/bin/env node\n")]
     [(spidermonkey)  (make-backend 170 #t #t #f "#!/usr/bin/env js\n")]
     [else (error 'backend "unknown backend")])))

(define debug-continuations? (make-parameter #f))

(define max-tail-calls 1)

(define single-tail-call? (= 1 max-tail-calls))

(provide (struct-out backend)
         current-backend
         debug-continuations?
         max-tail-calls
         single-tail-call?)
