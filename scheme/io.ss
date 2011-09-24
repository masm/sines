#lang s-exp "../base-lang.rkt"

(require
 "format.ss"
 "vector.ss"
 "../es/oop.ss"
 "../es/to-string.ss"
 "../es/bridge.ss")

;; None of the functions that work on files are implemented.

(define +eof-object+ (es-object))

(define (eof-object)
  +eof-object+)

(define (eof-object? obj)
  (eq? +eof-object+ obj))

(define-prototype port ())

(define-generic (close-port port))

(define (call-with-port port proc)
  (let ([x (proc port)])
    (close-port port)
    x))

(provide eof-object
         eof-object?
         call-with-port)

;;; Input ports

(define-prototype [input-port port]
  ())

(define-generic (port-eof? input-port))

(define-generic (get-char textual-input-port))
(define-generic (lookahead-char textual-input-port))

(define-prototype [initial-input-port input-port]
  ())

(define-prototype-method (get-char [self initial-input-port])
  (eof-object))

(define-prototype-method (lookahead-char [self initial-input-port])
  (eof-object))

(define current-input-port
  (let ([port (make-initial-input-port)])
    (case-lambda
      [() port]
      [(in) (set! port in)])))

(define (close-input-port port)
  (close-port port))

;;; Fix this code and activate it

(define (get-string-n textual-input-port count)
  (cond [(zero? count) ""]
        [else
         (let ([chars '()])
           (do [(c 0 (+ c 1))]
               [(or (port-eof? textual-input-port) ; FIXME: remove use of [port-eof?]
                    (= c count))]
             (set! chars (cons (get-char textual-input-port) chars)))
           (if (null? chars) (eof-object) (list->string (reverse chars))))]))

(define (get-string-all textual-input-port)
  (let ([chars '()])
    (do [(c (get-char textual-input-port) (get-char textual-input-port))]
        [(eof-object? c)]
      (set! chars (cons c chars)))
    (if (null? chars) (eof-object) (list->string (reverse chars)))))

(define (get-line textual-input-port)
  (let ([chars '()])
    (do [(c (get-char textual-input-port) (get-char textual-input-port))]
        [(or (eof-object? c)
             (char=? #\Linefeed c))
         (if (and (null? chars) (eof-object? c)) (eof-object) (list->string (reverse chars)))]
      (set! chars (cons c chars)))))

(define (read-char [textual-input-port (current-input-port)])
  (get-char textual-input-port))

(define (peek-char [textual-input-port (current-input-port)])
  (lookahead-char textual-input-port))

(define (read [textual-input-port (current-input-port)])
  (get-datum textual-input-port))

(define (get-datum textual-input-port)
  (let ([port textual-input-port])
    (define (read c)
      (let ([c (drop-white-space c)])
        (if (eof-object? c)
            (eof-object)
            (case c
              [(#\)) (error 'get-datum "Unexpected ')' found in stream.")]
              [(#\() (read-delimited-list)]
              [(#\") (read-string)]
              [(#\#) (read-after-hash)]
              [(#\') (let ([expr (read (get-char port))])
                       (when (eof-object? expr)
                         (error 'get-datum "Unexpected EOF while reading a QUOTE expression"))
                       (list 'quote expr))]
              [else (retrieve-object (read-atom c))]))))
    (define (read-after-hash)
      (let ((c (get-char port)))
        (if (eof-object? c)
            (error 'get-datum "EOF found after #")
            (case c
              [(#\F #\f) #f]
              [(#\T #\t) #t]
              [(#\\) (let ([c (get-char port)])
                       (if (eof-object? c)
                           (error 'get-datum "EOF found after #\\")
                           c))]
              [(#\() (list->vector (read-delimited-list))]
              (else (error 'get-datum "Unexpected char after #"))))))
    (define (read-atom first-char)
      (let ([chars (list first-char)])
        (do [(c (lookahead-char port) (lookahead-char port))]
            [(or (eof-object? c)
                 (white-space? c)
                 (char=? #\( c)
                 (char=? #\) c)
                 (char=? #\" c)
                 (char=? #\' c))
             (list->string (reverse chars))]
          (get-char port)               ; take char from stream
          (when (char=? #\\ c)
            (set! c (get-char port))
            (when (eof-object? c)
              (error 'get-datum "EOF found while reading an atom")))
          (set! chars (cons c chars)))))
    (define (read-delimited-list)
      (let ([list '()])
        (do [(c (drop-white-space (get-char port)) (drop-white-space (get-char port)))]
            [(or (eof-object? c) (char=? #\) c))
             (if (eof-object? c)
                 (error 'get-datum "Expected ')' not found in stream.")
                 (reverse list))]
          (set! list (cons (read c) list)))))
    (define (read-string)
      (let ((chars '()))
        (do ((c (get-char port) (get-char port)))
            ((or (eof-object? c)
                 (char=? #\" c))
             (if (eof-object? c)
                 (error 'get-datum "EOF found while reading a string")
                 (list->string (reverse chars))))
          (when (char=? #\\ c)
            (set! c (get-char port))
            (when (eof-object? c)
              (error 'get-datum "EOF found while reading a string")))
          (set! chars (cons c chars)))))
    (define (white-space? c)
      (or (char=? #\Space c)
          (char=? #\Linefeed c)
          (char=? #\Return c)
          (char=? #\Tab c)
          (char=? #\Page c)))
    (define (drop-white-space c)
      (do [(c c (get-char port))]
          [(or (eof-object? c) (not (white-space? c)))
           c]))
    (define (retrieve-object string)
      (or (string->number string) (string->symbol string)))
    (read (get-char port))))

(provide read)

;;; Output ports

(define-prototype [output-port port]
  ())

(define-generic (print-string-method textual-output-port string))
(define-generic (put-char textual-output-port char))

(define-prototype [initial-output-port output-port]
  (vector))

(define-prototype-method (print-string-method [self initial-output-port] string)
  (vector-push! (initial-output-port-vector self) string))

(define-prototype-method (put-char [self initial-output-port] char)
  (vector-push! (initial-output-port-vector self) (string char)))

(define current-output-port
  (let ([port (make-initial-output-port (vector))]
        [changed? #f])
    (case-lambda
      [() port]
      [(out)
       (unless changed?
         (set! changed? #t)
         (vector-for-each (lambda (s) (print-string-method out s))
                          (initial-output-port-vector port)))
       (set! port out)])))

(define current-error-port
  (let ([port (make-initial-output-port (vector))]
        [changed? #f])
    (case-lambda
      [() port]
      [(out)
       (unless changed?
         (set! changed? #t)
         (vector-for-each (lambda (s) (print-string-method out s))
                          (initial-output-port-vector port)))
       (set! port out)])))

(provide output-port output-port?
         print-string-method
         put-char
         current-output-port current-error-port)

;;;

(define-generic (flush-output-port output-port))

(define (put-string textual-output-port string [start 0] [count (- (string-length string) start)])
  (assert (<= (+ start count) (string-length string)))
  (do ((c 0 (+ c 1))
       (i start (+ i 1)))
      ((= c count))
    (put-char textual-output-port (string-ref string i))))

(define (display arg [out (current-output-port)])
  (print-string-method out (display-to-string arg)))

(define (put-datum out arg)
  (print-string-method out (write-to-string arg)))

(define (newline [out (current-output-port)])
  (print-string-method out "\n"))

(define (write-char char [textual-output-port (current-output-port)])
  (put-char textual-output-port char))

(define (write obj [textual-output-port (current-output-port)])
  (put-datum textual-output-port obj))

(define (close-output-port port)
  (close-port port))


(provide display
         write
         newline
         format)

;;;;; String ports

;;; String input port

(define-prototype [string-input-port input-port]
  ([buffer]
   [buffer-index #:mutable]
   [buffer-length]))

(define-prototype-method (get-char [port string-input-port])
  (let ([i (string-input-port-buffer-index port)])
    (cond [(< i (string-input-port-buffer-length port))
           (string-input-port-buffer-index-set! port (add1 i))
           (string-ref (string-input-port-buffer port) i)]
          [else (eof-object)])))

(define-prototype-method (lookahead-char [port string-input-port])
  (let ([i (string-input-port-buffer-index port)])
    (if (< i (string-input-port-buffer-length port))
        (string-ref (string-input-port-buffer port) i)
        (eof-object))))

(define-prototype-method (port-eof? [port string-input-port])
  (= (string-input-port-buffer-index port) (string-input-port-buffer-length port)))

(define-prototype-method (close-port (port string-input-port))
  (void))

(define (open-string-input-port string)
  (make-string-input-port string 0 (string-length string)))

(provide open-string-input-port)

;;; String output port

(define-prototype [string-output-port output-port]
  ([buffer #:mutable]))

(define-prototype-method (print-string-method [port string-output-port] str)
  (vector-push! (string-output-port-buffer port) str))

(define-prototype-method (put-char [port string-output-port] char)
  (vector-push! (string-output-port-buffer port) (string char)))

(define-prototype-method (flush-output-port [port string-output-port])
  (void))

(define-prototype-method (close-port (port string-output-port))
  (void))

(define (open-string-output-port)
  (make-string-output-port (vector)))

(define (get-output-port-string port)
  (let ([v (string-output-port-buffer port)])
    (string-output-port-buffer-set! port (vector))
    (vector-string-join v "")))

(define (call-with-string-output-port proc)
  (call-with-port (open-string-output-port) proc))

(provide open-string-output-port
         get-output-port-string
         call-with-string-output-port)

;;; The functions [load], [transcript-on], and [transcript-off] are not implemented.

;;;;; Unused code

#;
(define (display obj [textual-output-port (current-output-port)])
  (let ((port textual-output-port))
    (define (display obj)
      (cond ((null? obj) (put-string port "()"))
            ((pair? obj) (display-pair obj))
            ((string? obj) (put-string port obj))
            ((boolean? obj) (put-string port (if obj "#t" "#f")))
            ((number? obj) (put-string port (number->string obj)))
            ((symbol? obj) (put-string port (symbol->string obj)))
            ((char? obj) (put-char port obj))
            ((vector? obj) (display-vector obj))
            ((procedure? obj) (put-string port "#<procedure>"))
            ((port? obj) (put-string port "#<port>"))
            (else (error "Unkown object type"))))
    (define (display-char char)
      (put-string port "#\\")           ; FIXME: only works for visible chars
      (put-char port char))
    (define (display-pair pair)
      (put-char port #\()
      (display (car pair))
      (let ((rest (cdr pair)))
        (unless (null? rest)
          (if (pair? rest)
              (do ((rest rest (cdr rest)))
                  ((not (pair? rest))
                   (unless (null? rest)
                     (put-string port " . ")
                     (display rest)))
                (put-char port #\Space)
                (display (car rest)))
              (begin
                (put-string port " . ")
                (display rest)))))
      (put-char port #\)))
    (define (display-vector vector)
      (put-string port "#(")
      (let ((len (vector-length vector)))
        (unless (zero? len)
          (display (vector-ref vector 0))
          (do ((i 1 (+ i 1)))
              ((= i len))
            (put-char port #\Space)
            (display (vector-ref vector i)))))
      (put-char port #\)))
    (display obj)))

#;
(define (put-datum textual-output-port obj)
  (let ((port textual-output-port))
    (define (write obj)
      (cond [(null? obj) (put-string port "()")]
            [(pair? obj) (write-pair obj)]
            [(string? obj) (write-string obj)]
            [(boolean? obj) (put-string port (if obj "#t" "#f"))]
            [(number? obj) (put-string port (number->string obj))]
            [(symbol? obj) (write-symbol obj)]
            [(char? obj) (write-char obj)]
            [(vector? obj) (write-vector obj)]
            [(procedure? obj) (put-string port "#<procedure>")]
            [(port? obj) (put-string port "#<port>")]
            [else (error "Unkown object type")]))
    (define (write-char char)
      (put-string port "#\\")           ; FIXME: only works for visible chars
      (put-char port char))
    (define (write-pair pair)
      (put-char port #\()
      (write (car pair))
      (let ((rest (cdr pair)))
        (unless (null? rest)
          (if (pair? rest)
              (do ((rest rest (cdr rest)))
                  ((not (pair? rest))
                   (unless (null? rest)
                     (put-string port " . ")
                     (write rest)))
                (put-char port #\Space)
                (write (car rest)))
              (begin
                (put-string port " . ")
                (write rest)))))
      (put-char port #\)))
    (define (write-string string)
      (put-char port #\")
      (let ((len (string-length string)))
        (do ((i 0 (+ i 1)))
            ((= i len))
          (let ((c (string-ref string i)))
            (when (or (char=? #\\ c) (char=? #\" c)) ; FIXME: escape more chars
              (put-char port #\\))
            (put-char port c))))
      (put-char port #\"))
    (define (write-symbol symbol)
      (let ((string (symbol->string symbol)))
        (let ((len (string-length string)))
          (do ((i 0 (+ i 1)))
              ((= i len))
            (let ((c (string-ref string i)))
              (when (or (char=? #\\ c) (char=? #\" c)) ; FIXME: escape more chars
                (put-char port #\\))
              (put-char port c))))))
    (define (write-vector vector)
      (put-string port "#(")
      (let ((len (vector-length vector)))
        (unless (zero? len)
          (write (vector-ref vector 0))
          (do ((i 1 (+ i 1)))
              ((= i len))
            (put-char port #\Space)
            (write (vector-ref vector i)))))
      (put-char port #\)))
    (write obj)))

#;
(define (get-string-n! textual-input-port string start count)
  (assert (<= (+ start count) (string-length string)))
  (cond [(zero? count) 0]
        [else
         (do [(c 0 (+ c 1))
              (i start (+ i 1))]
             [(or (port-eof? textual-input-port) ; FIXME: remove use of [port-eof?]
                  (= c count))
              (if (zero? c) (eof-object) c)]
           (string-set! string i (get-char textual-input-port)))]))
