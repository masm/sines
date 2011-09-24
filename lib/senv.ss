#lang s-exp "../base-lang.ss"

(require
 "sdict.ss"
 "../es/oop.ss")

(define-prototype env
  (frame next))

(define empty-senv #f)

(define (senv-key? env str)
  (and (env? env)
       (or (sdict-key? (env-frame env) str)
           (senv-key? (env-next env) str))))

(define (senv-ref env str)
  (if (env? env)
      (if (sdict-key? (env-frame env) str)
          (sdict-ref (env-frame env) str)
          (senv-ref (env-next env) str))
      (error 'senv-ref "unknown binding" str)))

(define (senv-set! env str value)
  (if (env? env)
      (if (sdict-key? (env-frame env) str)
          (sdict-ref (env-frame env) str)
          (senv-ref (env-next env) str))
      (error 'senv-set! "unknown-binding" str)))

(define (extended-senv env new-dict)
  (make-env new-dict env))

(provide empty-senv
         senv-key?
         senv-ref
         senv-set!
         extended-senv)
