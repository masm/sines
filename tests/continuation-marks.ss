#lang planet masm/sines

(require
 (rename-in "../test.ss"
            [check-expect ->e]
            [check-expect-one-of ->e/one-of]
            [check-quoted-expect ->]
            [check-quoted-expect-one-of ->/one-of]))

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list (current-continuation-marks) key))

(begin
  (define toplevel-fact-marks #f)

  (define (toplevel-fact x)
    (with-continuation-mark 'args x
      (if (zero? x)
          (begin
            (set! toplevel-fact-marks (extract-current-continuation-marks 'args))
            1)
          (* x (toplevel-fact (sub1 x))))))

  [(toplevel-fact 3)   . -> . 6]
  [toplevel-fact-marks . -> . (0 1 2 3)])

(begin
  (define toplevel-fact-iter-marks #f)

  (define (toplevel-fact-iter x acc)
    (with-continuation-mark 'args x
      (if (zero? x)
          (begin
            (set! toplevel-fact-iter-marks (extract-current-continuation-marks 'args))
            acc)
          (toplevel-fact-iter (sub1 x) (* acc x)))))

  [(toplevel-fact-iter 3 1)  . -> . 6]
  [toplevel-fact-iter-marks  . -> . (0)])

;;;

[(with-continuation-mark 'key 'mark
   (extract-current-continuation-marks 'key))       . -> . (mark)]

[(with-continuation-mark 'key1 'mark1
   (with-continuation-mark 'key2 'mark2
     (list
      (extract-current-continuation-marks 'key1)
      (extract-current-continuation-marks 'key2)))) . -> . ((mark1) (mark2))]

[(with-continuation-mark 'key 'mark1
   (with-continuation-mark 'key 'mark2  ; replaces previous mark
     (extract-current-continuation-marks 'key)))    . -> . (mark2)]

[(with-continuation-mark 'key 'mark1
   (list                                ; continuation extended to evaluate the argument
    (with-continuation-mark 'key 'mark2
      (extract-current-continuation-marks 'key))))  . -> . ((mark2 mark1))]

[(let loop ([n 10])
   (if (zero? n)
       (extract-current-continuation-marks 'key)
       (with-continuation-mark 'key n
         (loop (sub1 n)))))                         . -> . (1)]

;;;

(let ([marks-2-1 #f])
  (define (g)
    (with-continuation-mark 'k '2
      (set! marks-2-1 (continuation-mark-set->list (current-continuation-marks) 'k)))
    (with-continuation-mark 'k '2
      (continuation-mark-set->list (current-continuation-marks) 'k)))
  (let ([f (lambda ()
             (with-continuation-mark 'k '1
               (g)))])
    [(f)                                            . -> . (2)]
    [marks-2-1                                      . -> . (2 1)]))

(let loop ([i 10000])
  (with-continuation-mark 'tail-call-key i
    (if (zero? i)
        [(continuation-mark-set->list (current-continuation-marks) 'tail-call-key) . -> . (0)]
        (loop (sub1 i)))))

;;;

;; (define (breakpoint x) x)

;; (define (fact x)
;;   (if (zero? x)
;;       (breakpoint 1)
;;       (* x (fact (sub1 x)))))

;; (fact 3)

;; (define (fact-acc x acc)
;;   (if (zero? x)
;;       acc
;;       (fact-acc (sub1 x) (* x acc))))

;; (fact-acc 5 1)


;; ;; (define ((f+ x) y)
;; ;;   (if (zero? x)
;; ;;       y
;; ;;       ((f+ (sub1 x)) (add1 y))))

;; ;; (define (fact+ x)
;; ;;   (if (zero? x)
;; ;;       (begin
;; ;;         (breakpoint 42)
;; ;;         1)
;; ;;       ((f+ x) (fact+ (sub1 x)))))

;; ;; (fact+ 3)

;; ;; (define (g x)
;; ;;   (breakpoint x))

;; ;; (define (f x)
;; ;;   (g x))

;; ;; (define (? x)
;; ;;   #t)

;; ;; (if (? 41)
;; ;;     (breakpoint 42)
;; ;;     (breakpoint 43))

;; ;; (f 42)
