#lang planet masm/sines

;;; Queue

(define (make-queue)
  (make-vector 0))

(define (queue-empty? queue)
  (zero? (vector-length queue)))

(define (enqueue queue value)
  (vector-push! queue value))

(define (dequeue queue)
  (vector-shift! queue))

(provide make-queue
         queue-empty?
         enqueue
         dequeue)

;;; Priority queue

(define-prototype priority-queue
  (order-proc vector))

(define (_make-priority-queue fn)
  (make-priority-queue fn (make-vector 0)))

(define (pq-empty? pq)
  (vector-empty? (priority-queue-vector pq)))

(define (pq-enqueue pq value)
  (let ([proc (priority-queue-order-proc pq)]
        [vec (priority-queue-vector pq)])
    (let ([len (vector-length vec)])
      (let loop ([i 0])
        (cond [(= len i)
               (vector-push! vec value)]
              [(< (proc value (vector-ref vec i)) 0)
               (vector-splice! vec i 0 value)]
              [else (loop (add1 i))]))))
  pq)

(define (pq-dequeue pq)
  (vector-shift! (priority-queue-vector pq)))

(provide (rename-out (_make-priority-queue make-priority-queue))
         pq-empty?
         pq-enqueue
         pq-dequeue)
