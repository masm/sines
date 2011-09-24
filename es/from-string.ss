#lang s-exp "../base-lang.rkt"

(require
 "bridge.ss")

(define (from-string/char c)
  (string-append "#\\" (string c)))

(define (to-string/pair c recur)
  (define (fn l)
    (cond [(null? (cdr l)) (recur (car l))]
          [(pair? (cdr l)) (string-append (recur (car l)) " " (fn (cdr l)))]
          [else            (string-append (recur (car l)) " . " (recur (cdr l)))]))
  (string-append "(" (fn c) ")"))

(define (to-string/vector v recur)
  (let ([l (vector->list v)])
    (if (null? l) "#()" (string-append "#" (to-string/pair l recur)))))

(define (display-to-string obj)
  (cond [(null? obj)         "()"]
        [(es-undefined? obj) "#<undefined>"]
        [(symbol? obj)       (symbol->string obj)]
        [(pair? obj)         (to-string/pair obj display-to-string)]
        [(vector? obj)       (to-string/vector obj display-to-string)]
        [(char? obj)         (string obj)]
        [(boolean? obj)      (if obj "#t" "#f")]
        [(procedure? obj)    "#<procedure>"]
        [else                (es-call-property obj "toString")]))

(provide read-from-string)

(define (from-string string)
  (read-from-string-starting-at string 0))

(define (from-string-starting-at string i)
  (let ([i (drop-white-space c)])
    (if (= i (string-length string))
        (eof-object)
        (case c
          [(#\)) (error "Unexpected ')' found in stream.")]
          [(#\() (read-delimited-list)]
          [(#\") (read-string)]
          [(#\#) (read-after-hash)]
          [(#\') (let ([expr (read (get-char port))])
                   (when (eof-object? expr)
                     (error 'get-datum "Unexpected EOF while reading a QUOTE expression"))
                   (list 'quote expr))]
          [else (retrieve-object (read-atom c))]))))

(define (read c)
  )

(define (read-after-hash)
  (let ((c (get-char port)))
    (if (eof-object? c)
        (error "EOF found after #")
        (case c
          [(#\F #\f) #f]
          [(#\T #\t) #t]
          [(#\\) (let ([c (get-char port)])
                   (if (eof-object? c)
                       (error "EOF found after #\\")
                       c))]
          [(#\() (list->vector (read-delimited-list))]
          (else (error 'get-datum "Unexpected char after #"))))))
  (define (read-atom first-char)
    (let ((chars (list first-char)))
      (do ((c (lookahead-char port) (lookahead-char port)))
          ((or (eof-object? c)
               (white-space? c)
               (char=? #\( c)
               (char=? #\) c)
               (char=? #\" c)
               (char=? #\' c))
           (list->string (reverse chars)))
        (get-char port)                 ; take char from stream
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
    (or (string->number string)
        (string->symbol string)))
  (read (get-char port))

