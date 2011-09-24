#lang s-exp "../base-lang.rkt"

(require
 "../es/bridge.ss")

(define (vector-copy! dest dest-start src [src-start 0] [src-end (vector-length src)])
  (do ([i dest-start (add1 dest-start)]
       [j src-start (add1 src-start)])
      ((>= j src-end))
    (vector-set! dest i (vector-ref src j))))

(define (vector->values vec [start-pos 0] [end-pos (vector-length vec)])
  (apply values (vector->list (es-call-property vec "slice" start-pos end-pos))))

(define (build-vector n proc)
  (do ([i 0 (add1 i)]
       [l '() (cons (proc i) l)])
      ((>= i n) (list->vector (reverse l)))))

(define vector-append
  (case-lambda
    [(vec)
     vec]
    [(v1 v2 . vs)
     (apply vector-append (array-concat v1 v2) vs)]))

(define-invoker (array-concat array array2)
  "concat")

(define-invoker (array-slice array start end)
  "slice")

(define-invoker (array-slice1 array start)
  "slice")

(define (vector-take vec n)
  (array-slice vec 0 n))

(define (vector-take-right vec n)
  (array-slice1 vec (- n)))

(define (vector-drop vec n)
  (array-slice1 vec n))

(define (vector-drop-right vec n)
  (array-slice vec 0 (- n)))

(define (vector-split-at vec pos)
  (values (vector-take vec pos) (vector-drop vec pos)))

(define (vector-split-at-right vec pos)
  (values (vector-drop-right vec pos) (vector-take-right vec pos)))

(define (vector-copy vec [start 0] [end (vector-length vec)])
  (array-slice vec start end))

(define (vector-filter proc vec)
  (let ([v (vector)])
    (vector-for-each (lambda (w)
                       (when (proc w)
                         (vector-push! v w)))
                     vec)
    v))

(define (vector-filter-map proc v)
  (vector-filter (lambda (x) x) (vector-map proc v)))

(define (vector-filter-not proc vec)
  (let ([v (vector)])
    (vector-for-each (lambda (w)
                       (unless (proc w)
                         (vector-push! v w)))
                     vec)
    v))

(define (vector-memp pred? vec)
  (let ([len (vector-length vec)])
    (let loop ([i 0])
      (cond [(= i len)                  #f]
            [(pred? (vector-ref vec i)) i]
            [else (loop (add1 i))]))))

(define (vector-memq obj vec)
  (vector-memp (lambda (x) (eq? obj x)) vec))

(define (vector-memv obj vec)
  (vector-memp (lambda (x) (eqv? obj x)) vec))

(define (vector-member obj vec)
  (vector-memp (lambda (x) (equal? obj x)) vec))

(define (vector-find pred? vec)
  (let ([i (vector-memp pred? vec)])
    (and i (vector-ref vec i))))

(define vector-for-all
  (case-lambda
    [(proc vec)
     (let ([len (vector-length vec)])
       (let loop ([i 0])
         (cond [(= i len) #t]
               [else (and (proc (vector-ref vec i)) (loop (add1 i)))])))]
    [(proc vec . vecs)
     (error 'vector-for-all "not implemented")]))

(provide vector-copy!
         vector->values
         build-vector
         vector-append
         vector-take
         vector-take-right
         vector-drop
         vector-drop-right
         vector-split-at
         vector-split-at-right
         vector-copy
         vector-filter
         vector-filter-map
         vector-filter-not
         vector-memp
         vector-memq
         vector-memv
         vector-member
         vector-find
         vector-for-all)

;;;

(define-invoker (vector-reverse! vec)
  "reverse")

(define-invoker (vector-push! vec item . items)
  "push")

(define-invoker (vector-pop! vec)
  "pop")

(define-invoker (vector-string-join array separator)
  "join")

(provide vector-reverse!
         vector-push!
         vector-pop!
         vector-string-join)

;;;

(define (vector-empty? v)
  (zero? (vector-length v)))

(define (empty-vector? v)
  (and (vector? v)
       (vector-empty? v)))

(provide vector-empty?
         empty-vector?)

;;;

(define vector-map-with-index
  (let ()
    (define (map1 proc vec)
      (let ([len (vector-length vec)])
        (let ([new-vec (make-vector len)])
          (let loop ([i 0])
            (if (= len i)
                new-vec
                (begin
                  (vector-set! new-vec i (proc i (vector-ref vec i)))
                  (loop (add1 i))))))))
    (define (map-many proc vecs)
      (let ([len (vector-length (car vecs))])
        (let ([new-vec (make-vector len)])
          (let loop ([i 0])
            (if (= len i)
                new-vec
                (begin
                  (vector-set! new-vec i
                               (apply proc i (map (lambda (v)
                                                  (vector-ref v i))
                                                vecs)))
                  (loop (add1 i))))))))
    (lambda (proc vector . vectors)
      (if (null? vectors)
          (map1 proc vector)
          (map-many proc (cons vector vectors))))))

(provide vector-map-with-index)

(define vector-for-each-with-index
  (let ()
    (define (fn1 proc vec)
      (let ([len (vector-length vec)])
        (let loop ([i 0])
          (unless (= len i)
            (begin
              (proc i (vector-ref vec i))
              (loop (add1 i)))))))
    (define (fn-many proc vecs)
      (let ([len (vector-length (car vecs))])
        (let loop ([i 0])
          (unless (= len i)
            (apply proc i (map (lambda (v)
                                 (vector-ref v i))
                               vecs))
            (loop (add1 i))))))
    (lambda (proc vector . vectors)
      (if (null? vectors)
          (fn1 proc vector)
          (fn-many proc (cons vector vectors))))))

(provide vector-for-each-with-index)

;;;

(define-invoker (vector-splice! array start delete-count . items) "splice")

(define-invoker (vector-shift! array) "shift")

(define-invoker (vector-unshift! array . items) "unsift")


(provide
 vector-splice!
 vector-shift!
 vector-unshift!
 )

;;;

(define-invoker (_vector-sort! array [compare-fn]) "sort")

(define vector-sort!
  (case-lambda
    [(vec)      (_vector-sort! vec)]
    [(vec proc) (_vector-sort! vec (wrap-for-callback proc))]))

(provide vector-sort!)
