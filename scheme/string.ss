#lang s-exp "../base-lang.rkt"

(require
 "../lib/regexp.ss"
 "../es/bridge.ss"
 "vector.ss")

(define (string-starts-with? str other-str)
  (string=? (substring str 0 (string-length other-str))
            other-str))

(define (string-ends-with? str other-str)
  (string=? (es-call-property str "slice" (- (string-length other-str)))
            other-str))

(provide string-starts-with?
         string-ends-with?)

(define (string-empty? s)
  (string=? s ""))

(define (empty-string? s)
  (and (string? s) (string-empty? s)))

(provide string-empty?
         empty-string?)

(define (first-line str)
  (let ([r (regexp-match (regexp "^([^\n]+)") str)])
    (and r (cadr r))))

(provide first-line)

(define string-map
  (let ()
    (define (map1 proc str)
      (let ([len (string-length str)])
        (let ([new-vec (make-vector len)])
          (do [(i 0 (add1 i))]
              [(= len i) (vector-string-join new-vec "")]
             (vector-set! new-vec i (string (proc (string-ref str i))))))))
    (case-lambda
      [(proc str)
       (map1 proc str)]
      [(proc str1 str2)
       (let ([len (string-length str1)])
         (let ([new-vec (make-vector len)])
           (do [(i 0 (add1 i))]
               [(= len i) (vector-string-join new-vec "")]
             (vector-set! new-vec i (string (proc (string-ref str1 i) (string-ref str2 i)))))))]
      [(proc . strs)
       (let ([len (string-length (car strs))])
         (let ([new-vec (make-vector len)])
           (do [(i 0 (add1 i))]
               [(= len i) (vector-string-join new-vec "")]
             (vector-set! new-vec i (string (apply proc (map (lambda (v) (string-ref v i)) strs)))))))])))


(provide string-map)

(define-invoker (string-trim str)
  "trim")

(provide
 string-trim)

(define (string-map->list proc str)
  (map proc (string->list str)))

(define (string-map->vector proc str)
  (vector-map proc (list->vector (string->list str))))

(provide
 string-map->list
 string-map->vector)
