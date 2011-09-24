#lang s-exp "sines-lang.rkt"

(define-syntax-rule (until test e1 e2 ...)
  (do () (test) e1 e2 ...))

(define-syntax-rule (while test e1 e2 ...)
  (until (not test) e1 e2 ...))

(define (make-generator f)
  (let ([caller #f])
    (define (lcs)
      (f yield))
    (define (yield val)
      (let/cc k
              (set! lcs k)
              (caller val)))
    (lambda ()
      (let/cc k
              (set! caller k)
              (lcs)))))

(define-syntax-rule (define-generator yield (name args ...) e1 e2 ...)
  (define (name args ...)
    (make-generator (lambda (yield) e1 e2 ...))))

(define (filter/g pred generate)
  (make-generator (lambda (yield)
                    (do ([v (generate) (generate)])
                        (#f)
                      (when (pred v)
                        (yield v))))))

(define (map/g proc generate)
  (make-generator (lambda (yield)
                    (do ([v (generate) (generate)])
                        (#f)
                      (yield (proc v))))))

(define (generator->stream generate)
  (let loop ([v (generate)])
    (cons v (loop (generate)))))

(provide until while
         define-generator make-generator)
