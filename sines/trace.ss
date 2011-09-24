#lang s-exp "../sines-lang.rkt"

(define _trace
  (let ([indent 0])
    (lambda (name proc)
      (lambda args
        (let/cc k
          (let ([is (make-string indent  #\space)])
            (display (format "~A-> ~A : ~S\n" is name args))
            (set! indent (+ indent 3))
            (call-with-values (lambda () (apply proc args))
              (lambda results
                (set! indent (- indent 3))
                (display (format "~A<- ~A : ~S\n" is name results))
                (apply k results)))))))))

(define-syntax (trace stx)
  (syntax-case stx ()
    [(_ name obj)
     (identifier? #'name)
     #'(_trace (symbol->string 'name) obj)]
    [(_ name)
     (identifier? #'name)
     #'(set! name (_trace (symbol->string 'name) name))]
    [(_ obj)
     #'(_trace "<unknown>" obj)]))

(provide trace)