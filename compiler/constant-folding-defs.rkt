#lang scheme/base

(require scheme/base
         scheme/contract
         scheme/list
         (rename-in scheme/match
                    [match scheme-match])
         "primitives.rkt"
         "syntax.rkt"
         "store.rkt"
         (prefix-in s: "syntax2.rkt")
         (prefix-in p: "primitives.rkt")
         (prefix-in p: "primitives2.rkt")
         (prefix-in l: "../private/library.ss"))

(define-syntax-rule (match expr clause ...)
  (scheme-match expr clause ... [_ #f]))

(define constant-folding-procs (make-hasheq))
(define constant-folding-procs2 (make-hash))

(define (apply-constant-folder node)
  (let loop ([node node] [previous-node node])
    (match node
      [(s:primapp op args loc)
       (loop (really-apply-constant-folder op loc args) node)]
      [(s:app (s:global-ref id) args loc)
       (loop (really-apply-constant-folder2 id loc args) node)]
      [#f previous-node]
      [_  node])))

(define (really-apply-constant-folder op loc arguments)
  (cond [(hash-ref constant-folding-procs op (lambda () #f))
         => (lambda (proc) (apply proc loc arguments))]
        [else #f]))

(define (really-apply-constant-folder2 id loc arguments)
  (cond [(hash-ref constant-folding-procs2 id (lambda () #f))
         => (lambda (proc) (apply proc loc arguments))]
        [else #f]))

(define-syntax-rule (define-constant-folder (name loc . args) expr1 expr2 ...)
  (add-constant-folder name (lambda (loc . args) expr1 expr2 ...)))

(define-syntax-rule (define-constant-folder2 (var loc . args) expr1 expr2 ...)
  (add-constant-folder2 (sines-variable-id var) (lambda (loc . args) expr1 expr2 ...)))

(define (add-constant-folder name proc)
  (if (hash-has-key? constant-folding-procs name)
      (error 'add-constant-folder "redefining ~S" name)
      (hash-set! constant-folding-procs name proc)))

(define (add-constant-folder2 id proc)
  (if (hash-has-key? constant-folding-procs2 id)
      (error 'add-constant-folder2 "redefining ~S" id)
      (hash-set! constant-folding-procs2 id proc)))

(provide apply-constant-folder)

(define (emit-error name str . args)
  (s:primapp p:%%error (list (s:literal (format "~S: ~A" name (apply format str args))))))

;;;;; Constant folding definitions

(define-match-expander expr
  (syntax-rules ()
    [(_ expr)
     (? (lambda (x) (equal? x expr)))]))

;;;

(define-constant-folder (p:%%eq? loc x y)
  (match x
    [(s:literal x)
     (match y
       [(s:literal y)                       (s:literal (eq? x y) loc)]
       [(or (s:lambda) (s:dispatch-lambda)) (s:literal #f loc)])]
    [(or (s:lambda) (s:dispatch-lambda))
     (match y
       [(s:literal y) (s:literal #f loc)])]))

;;;

(define-constant-folder (p:%%procedure? loc x)
  (match x
    [(s:literal)                         (s:literal #f loc)]
    [(or (s:lambda) (s:dispatch-lambda)) (s:literal #t loc)]))

;;;

(define-constant-folder (p:%%number? loc x)
  (match x
    [(s:literal x)                       (s:literal (real? x) loc)]
    [(or (s:lambda) (s:dispatch-lambda)) (s:literal #f loc)]))

(define-constant-folder (p:%%= loc x y)
  (match x
    [(s:literal x xloc)
     (if (real? x)
         (match y
           [(s:literal y yloc)
            (if (real? y)
                (s:literal (= x y) loc)
                (emit-error '%%= "takes numbers as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%= "takes numbers as arguments, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%< loc x y)
  (match x
    [(s:literal x xloc)
     (if (real? x)
         (match y
           [(s:literal y yloc)
            (if (real? y)
                (s:literal (< x y) loc)
                (emit-error '%%< "takes numbers as arguments, given ~S" y yloc))])
         (emit-error '%%< "takes numbers as arguments, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%<= loc x y)
  (match x
    [(s:literal x xloc)
     (if (real? x)
         (match y
           [(s:literal y yloc)
            (if (real? y)
                (s:literal (<= x y) loc)
                (emit-error '%%<= "takes numbers as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%<= "takes numbers as arguments, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%+ loc x y)
  (match x
    [(s:literal x xloc)
     (if (real? x)
         (cond [(zero? x) y]
               [else (match y
                       [(s:literal y yloc)
                        (if (real? y)
                            (s:literal (+ x y) loc)
                            (emit-error '%%+ "takes numbers as arguments, given ~S, at ~S" y yloc))]
                       [(s:primapp (expr p:%%+) (list (s:literal y yloc) z) loc2)
                        (if (real? y)
                            (s:primapp p:%%+ (list (s:literal (+ x y) xloc) z) loc2)
                            (emit-error '%%+ "takes numbers as arguments, given ~S, at ~S" y yloc))]
                       [(s:primapp (expr p:%%-) (list (s:literal y yloc) z) loc2)
                        (if (real? y)
                            (s:primapp p:%%- (list (s:literal (+ x y) xloc) z) loc2)
                            (emit-error '%%- "takes numbers as arguments, given ~S, at ~S" y yloc))])])
         (emit-error '%%+ "takes numbers as arguments, given ~S, at ~S" x xloc))]
    [_
     (match y
       [(s:literal) (s:primapp p:%%+ (list y x) loc)])]))

(define-constant-folder (p:%%- loc x y)
  (match x
    [(s:literal x xloc)
     (if (real? x)
         (match y
           [(s:literal y yloc)
            (if (real? y)
                (s:literal (- x y) xloc)
                (emit-error '%%- "takes numbers as arguments, given ~S, at ~S" y yloc))]
           [(s:primapp (expr p:%%+) (list (s:literal y yloc) z) loc2)
            (if (real? y)
                (s:primapp p:%%- (list (s:literal (- x y) xloc) z) loc2)
                (emit-error '%%+ "takes numbers as arguments, given ~S, at ~S" y yloc))]
           [(s:primapp (expr p:%%-) (list (s:literal y yloc) z) loc2)
            (if (real? y)
                (s:primapp p:%%+ (list (s:literal (- x y) xloc) z) loc2)
                (emit-error '%%- "takes numbers as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%- "takes numbers as arguments, given ~S, at ~S" x xloc))]
    [_
     (match y
       [(s:literal y yloc)
        (if (real? y)
            (s:primapp p:%%+ (list (s:literal (- y) yloc) x) loc)
            (emit-error '%%- "takes numbers as arguments, given ~S, at ~S" y yloc))])]))

(define-constant-folder (p:%%* loc x y)
  (match x
    [(s:literal x xloc)
     (if (real? x)
         (cond [(= 1 x) y]
               [else (match y
                       [(s:literal y yloc)
                        (if (real? y)
                            (s:literal (* x y) xloc)
                            (emit-error '%%* "takes numbers as arguments, given ~S, at ~S" y yloc))]
                       [(s:primapp (expr p:%%*) (list (s:literal y yloc) z) loc2)
                        (if (real? y)
                            (s:primapp p:%%* (list (s:literal (* x y) xloc) z) loc2)
                            (emit-error '%%* "takes numbers as arguments, given ~S, at ~S" y yloc))]
                       [(s:primapp (expr p:%%/) (list (s:literal y yloc) z) loc2)
                        (if (real? y)
                            (s:primapp p:%%/ (list (s:literal (* x y) xloc) z) loc2)
                            (emit-error '%%/ "takes numbers as arguments, given ~S, at ~S" y yloc))])])
         (emit-error '%%* "takes numbers as arguments, given ~S, at ~S" x xloc))]
    [_
     (match y
       [(s:literal)
        (s:primapp p:%%* (list y x) loc)])]))

(define-constant-folder (p:%%/ loc x y)
  (match x
    [(s:literal x xloc)
     (if (real? x)
         (match y
           [(s:literal y yloc)
            (if (real? y)
                (if (zero? y)
                    (emit-error '%%/ "division by zero, at ~S" loc)
                    (s:literal (/ x y) xloc))
                (emit-error '%%/ "takes numbers as arguments, given ~S, at ~S" y yloc))]
           [(s:primapp (expr p:%%*) (list (s:literal y yloc) z) loc2)
            (if (real? y)
                (s:primapp p:%%/ (list (s:literal (/ x y) xloc) z) loc2)
                (emit-error '%%* "takes numbers as arguments, given ~S, at ~S" y yloc))]
           [(s:primapp (expr p:%%/) (list (s:literal y yloc) z) loc2)
            (if (real? y)
                (s:primapp p:%%* (list (s:literal (/ x y) xloc) z) loc2)
                (emit-error '%%/ "takes numbers as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%/ "takes numbers as arguments, given ~S, at ~S" x xloc))]
    [_
     (match y
       [(s:literal y yloc)
        (if (real? y)
            (s:primapp p:%%* (list (s:literal (/ 1 y) yloc) x) loc)
            (emit-error '%%/ "takes numbers as arguments, given ~S, at ~S" y yloc))])]))

(define-constant-folder (p:%%mod loc x y)
  (match x
    [(s:literal x)
     (match y
       [(s:literal y)
        (s:literal (remainder x y) loc)])]))

(define-constant-folder (p:%%bitwise-ior loc x y)
  (match x
    [(s:literal x)
     (match y
       [(s:literal y)
        (s:literal (bitwise-ior x y) loc)])]))

(define-constant-folder (p:%%bitwise-and loc x y)
  (match x
    [(s:literal x)
     (match y
       [(s:literal y)
        (s:literal (bitwise-and x y) loc)])]))

(define-constant-folder (p:%%bitwise-xor loc x y)
  (match x
    [(s:literal x)
     (match y
       [(s:literal y)
        (s:literal (bitwise-xor x y) loc)])]))

(define-constant-folder (p:%%bitwise-not loc x)
  (match x
    [(s:literal x) (s:literal (bitwise-not x) loc)]))

(define-constant-folder (p:%%shift-left loc x y)
  (match x
    [(s:literal x)
     (match y
       [(s:literal y)
        (s:literal (arithmetic-shift x y) loc)])]
    [_ (match y
         [(s:literal (? zero?)) x])]))

(define-constant-folder (p:%%shift-right loc x y)
  (match x
    [(s:literal x)
     (match y
       [(s:literal y)
        (s:literal (arithmetic-shift x (- y)) loc)])]
    [_ (match y
         [(s:literal (? zero?)) x])]))

;;;

(define-constant-folder (p:%%boolean? loc x)
  (match x
    [(s:literal x) (s:literal (boolean? x) loc)]
    [(or (s:lambda) (s:dispatch-lambda)) (s:literal #f loc)]))

(define-constant-folder (p:%%boolean=? loc x y)
  (match x
    [(s:literal x xloc)
     (if (boolean? x)
         (match y
           [(s:literal y yloc)
            (if (boolean? y)
                (s:literal (eq? x y) loc)
                (emit-error '%%boolean=? "takes booleans as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%boolean=? "takes booleans as arguments, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%not loc x)
  (match x
    [(s:literal x xloc) (s:literal (not x) loc)]
    [(or (s:lambda) (s:dispatch-lambda)) (s:literal #f loc)]))

;;;

(define-constant-folder (p:%%pair? loc x)
  (match x
    [(s:literal x) (s:literal (pair? x) loc)]
    [(or (s:lambda) (s:dispatch-lambda)) (s:literal #f loc)]))

(define-constant-folder (p:%%car loc x)
  (match x
    [(s:literal x xloc)
     (if (pair? x)
         (s:literal (car x) loc)
         (emit-error '%%car "takes a pair as argument, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%cdr loc x)
  (match x
    [(s:literal x xloc)
     (if (pair? x)
         (s:literal (cdr x) loc)
         (emit-error '%%cdr "takes a pair as argument, given ~S, at ~S" x xloc))]))

;;;

(define-constant-folder (p:%%symbol? loc x)
  (match x
    [(s:literal x) (s:literal (symbol? x) loc)]
    [(or (s:lambda) (s:dispatch-lambda)) (s:literal #f loc)]))

(define-constant-folder (p:%%symbol=? loc x y)
  (match x
    [(s:literal x xloc)
     (if (symbol? x)
         (match y
           [(s:literal y yloc)
            (if (symbol? y)
                (s:literal (eq? x y) loc)
                (emit-error '%%symbol=? "takes symbols as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%symbol=? "takes symbols as arguments, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%symbol->string loc x)
  (match x
    [(s:literal x xloc)
     (if (symbol? x)
         (s:literal (symbol->string x) loc)
         (emit-error '%%symbol->string "takes a symbol, given ~S, at ~S" x xloc))]))

;;;

(define-constant-folder (p:%%char? loc x)
  (match x
    [(s:literal x) (s:literal (char? x) loc)]
    [(or (s:lambda) (s:dispatch-lambda)) (s:literal #f loc)]))

(define-constant-folder (p:%%char->integer loc x)
  (match x
    [(s:literal x xloc)
     (if (char? x)
         (s:literal (char->integer x) loc)
         (emit-error '%%char->integer "takes a char, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%char=? loc x y)
  (match x
    [(s:literal x xloc)
     (if (char? x)
         (match y
           [(s:literal y yloc)
            (if (char? y)
                (s:literal (char=? x y) loc)
                (emit-error '%%char=? "takes chars as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%char=? "takes chars as arguments, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%char<? loc x y)
  (match x
    [(s:literal x xloc)
     (if (char? x)
         (match y
           [(s:literal y yloc)
            (if (char? y)
                (s:literal (char<? x y) loc)
                (emit-error '%%char<? "takes chars as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%char<? "takes chars as arguments, given ~S, at ~S" x xloc))]))

(define-constant-folder (p:%%char<=? loc x y)
  (match x
    [(s:literal x xloc)
     (if (char? x)
         (match y
           [(s:literal y yloc)
            (if (char? y)
                (s:literal (char<=? x y) loc)
                (emit-error '%%char<=? "takes chars as arguments, given ~S, at ~S" y yloc))])
         (emit-error '%%char<=? "takes chars as arguments, given ~S, at ~S" x xloc))]))

;;;

(define-constant-folder (p:%%string? loc x)
  (match x
    [(s:literal x) (s:literal (string? x) loc)]
    [(or (s:lambda) (s:dispatch-lambda)) (s:literal #f loc)]))

(define-constant-folder (p:%%string=? loc x y)
  (match x
    [(s:literal x)
     (if (string? x)
         (match y
           [(s:literal y)
            (if (string? y)
                (s:literal (string=? x y) loc)
                (emit-error '%%string=? "takes strings as arguments, given ~S" y))])
         (emit-error '%%string=? "takes strings as arguments, given ~S" x))]))

(define-constant-folder (p:%%string<? loc x y)
  (match x
    [(s:literal x)
     (if (string? x)
         (match y
           [(s:literal y)
            (if (string? y)
                (s:literal (string<? x y) loc)
                (emit-error '%%string<? "takes strings as arguments, given ~S" y))])
         (emit-error '%%string<? "takes strings as arguments, given ~S" x))]))

(define-constant-folder (p:%%string<=? loc x y)
  (match x
    [(s:literal x)
     (if (string? x)
         (match y
           [(s:literal y)
            (if (string? y)
                (s:literal (string<=? x y) loc)
                (emit-error '%%string<=? "takes strings as arguments, given ~S" y))])
         (emit-error '%%string<=? "takes strings as arguments, given ~S" x))]))

(define-constant-folder (p:%%string-length loc x)
  (match x
    [(s:literal x)
     (if (string? x)
         (s:literal (string-length x) loc)
         (emit-error '%%string-length "takes strings as arguments, given ~S" x))]))

(define-constant-folder (p:%%string-append loc x y)
  (match x
    [(s:literal xv)
     (if (string? xv)
         (match y
           [(s:literal yv)
            (if (string? yv)
                (s:literal (string-append xv yv) loc)
                (emit-error '%%string-append "takes strings as arguments, given ~S, at ~S" yv loc))]
           [(s:primapp (expr p:%%string-append) (list (s:literal yv) z) loc2)
            (if (string? yv)
                (s:primapp p:%%string-append (list (s:literal (string-append xv yv) loc2) z) loc)
                (emit-error '%%string-append "takes strings as arguments, given ~S, at ~S" yv loc2))])
         (emit-error '%%string-append "takes strings as arguments, given ~S, at ~S" xv loc))]))

;;;

(define-constant-folder (p:es-apply-to/list loc proc obj arg . args)
  (match args
    [(list init ... last)
     (match last
       [(s:literal l loc2)
        (if (list? l)
            (s:primapp p:es-call-property
                       (list* proc (s:literal "call") obj arg
                              ;; TODO: remove need for lambda, bellow
                              (append init (map (lambda (x) (s:literal x)) l)))
                       loc)
            (emit-error 'es-apply-to/list "takes a list as last argument, given ~S, at ~S" last loc2))])]))

;;;;; Non primitives

(define-constant-folder2 (l:= loc x y . args)
  (and (null? args) (s:primapp p:%%= (list x y) loc)))

(define-constant-folder2 (l:< loc x y . args)
  (and (null? args) (s:primapp p:%%< (list x y) loc)))

(define-constant-folder2 (l:> loc x y . args)
  (and (null? args) (s:primapp p:%%< (list y x) loc)))

(define-constant-folder2 (l:<= loc x y . args)
  (and (null? args) (s:primapp p:%%<= (list x y) loc)))

(define-constant-folder2 (l:>= loc x y . args)
  (and (null? args) (s:primapp p:%%<= (list y x) loc)))

;;;

(define-constant-folder2 (l:+ loc . args)
  (cond [(null? args) (s:literal 0 loc)]
        [else (fold-left (lambda (a b) (s:primapp p:%%+ (list a b) #f)) (car args) (cdr args))]))

(define-constant-folder2 (l:* loc . args)
  (cond [(null? args) (s:literal 1 loc)]
        [else (fold-left (lambda (a b) (s:primapp p:%%* (list a b) #f)) (car args) (cdr args))]))

(define-constant-folder2 (l:- loc arg . args)
  (cond [(null? args) (s:primapp p:%%- (list (s:literal 0 #f) arg) loc)]
        [else (fold-left (lambda (a b) (s:primapp p:%%- (list a b) #f)) arg args)]))

(define-constant-folder2 (l:/ loc arg . args)
  (cond [(null? args) (s:primapp p:%%/ (list (s:literal 1 #f) arg) loc)]
        [else (fold-left (lambda (a b) (s:primapp p:%%/ (list a b) #f)) arg args)]))

(define-constant-folder2 (l:bitwise-and loc . args)
  (cond [(null? args) 0]
        [else (fold-left (lambda (a b) (s:primapp p:%%bitwise-and (list a b) #f)) (car args) (cdr args))]))

(define-constant-folder2 (l:bitwise-ior loc . args)
  (cond [(null? args)]
        [else (fold-left (lambda (a b) (s:primapp p:%%bitwise-ior (list a b) #f)) (car args) (cdr args))]))

(define-constant-folder2 (l:bitwise-xor loc . args)
  (cond [(null? args)]
        [else (fold-left (lambda (a b) (s:primapp p:%%bitwise-xor (list a b) #f)) (car args) (cdr args))]))

;;;

(define-constant-folder2 (l:char=? loc x y . args)
  (and (null? args) (s:primapp p:%%char=? (list x y) loc)))

(define-constant-folder2 (l:char<? loc x y . args)
  (and (null? args) (s:primapp p:%%char<? (list x y) loc)))

(define-constant-folder2 (l:char>? loc x y . args)
  (and (null? args) (s:primapp p:%%char<? (list y x) loc)))

(define-constant-folder2 (l:char<=? loc x y . args)
  (and (null? args) (s:primapp p:%%char<=? (list x y) loc)))

(define-constant-folder2 (l:char>=? loc x y . args)
  (and (null? args) (s:primapp p:%%char<=? (list y x) loc)))

;;;

(define-constant-folder2 (l:string=? loc x y . args)
  (and (null? args) (s:primapp p:%%string=? (list x y) loc)))

(define-constant-folder2 (l:string<? loc x y . args)
  (and (null? args) (s:primapp p:%%string<? (list x y) loc)))

(define-constant-folder2 (l:string>? loc x y . args)
  (and (null? args) (s:primapp p:%%string<? (list y x) loc)))

(define-constant-folder2 (l:string<=? loc x y . args)
  (and (null? args) (s:primapp p:%%string<=? (list x y) loc)))

(define-constant-folder2 (l:string>=? loc x y . args)
  (and (null? args) (s:primapp p:%%string<=? (list y x) loc)))

(define-constant-folder2 (l:string-append loc . args)
  (cond [(null? args) (s:literal "" loc)]
        [else (fold-left (lambda (a b) (s:primapp p:%%string-append (list a b) loc)) (car args) (cdr args))]))

;;;

(define-constant-folder2 (l:length loc node)
  (match node
    [(s:literal x xloc)
     (if (list? x)
         (s:literal (length x) loc)
         (emit-error 'l:length "takes list as argument, given ~S, at ~S" x xloc))]))

(define-constant-folder2 (l:list loc . args)
  (foldr (lambda (a b) (s:primapp p:%%cons (list a b) loc))
         (s:literal '() loc)
         args))

;;;

(define-constant-folder2 (l:vector loc . args)
  (s:primapp p:es-array args loc))

;;;

(define-constant-folder2 (l:apply loc op . args)
  (match args
    [(list init ... last)
     (match last
       [(s:literal v loc2)
        (if (list? v)
            (s:app op (append init (map (lambda (v) (s:literal v loc2)) v)) loc)
            (emit-error 'apply "last argument must be a proper list, given ~S, at ~S " v loc2))]
       [(s:primapp p:%%cons (list a d) loc2)
        (s:app (s:global-ref (sines-variable-id l:apply) loc) (cons op (append init (list a d))) loc2)])]))

;;;

(define (fold-left proc s l)
  (let loop ([r s]
             [l l])
    (if (null? l) r (loop (proc r (car l)) (cdr l)))))