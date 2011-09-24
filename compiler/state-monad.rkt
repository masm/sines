#lang scheme/base

(require scheme/base)

(define (return node)
  (lambda (state)
    (cons node state)))

(define (>>= state-m proc)
  (lambda (state)
    (let ([pair (state-m state)])
      ((proc (car pair)) (cdr pair)))))

(define (>>=/sample state-m proc)
  (lambda (state)
    (let ([pair (state-m state)])
      ((proc (car pair) (cdr pair)) (cdr pair)))))

(define (return/reset node state-proc)
  (lambda (state)
    (cons node (state-proc state))))

(define (>>=/sample/reset state-m proc)
  (lambda (state)
    (let ([pair (state-m state)])
      (let-values ([(state-m2 state2)
                    (proc (car pair) (cdr pair))])
        (state-m2 state2)))))

(define-syntax-rule (let-m ([var val]) expr)
  (>>= val (lambda (var) expr)))

(define-syntax let*-m
  (syntax-rules ()
    [(_ () expr)
     expr]
    [(_ ([var val] more ...) expr)
     (let-m ([var val])
       (let*-m (more ...) expr))]))

(define (inject state-m state)
  (car (state-m state)))

(define (monad-seq transform l k)
  (if (null? l)
      (k '())
      (>>= (transform (car l))
           (let loop ([args (cdr l)] [nodes '()])
             (lambda (node)
               (if (null? args)
                   (k (reverse (cons node nodes)))
                   (>>= (transform (car args))
                        (loop (cdr args)
                              (cons node nodes)))))))))

(provide return >>= >>=/sample return/reset >>=/sample/reset
         inject monad-seq
         let-m let*-m)
