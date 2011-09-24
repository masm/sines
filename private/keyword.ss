#lang s-exp "bootstrap-lang.rkt"

(require
 (for-syntax racket/base)
 (for-syntax syntax/stx)
 "library.ss")

(define-for-syntax (keyword-stx-<? a b)
   (string<? (keyword->string (syntax-e a))
             (keyword->string (syntax-e b))))

(define-for-syntax (keyword-<? a b)
  (string<? (keyword->string a)
            (keyword->string b)))

(define-for-syntax (parse-params stx params-stx)
  (let loop ([params (stx->list params-stx)]
             [req-id    '()]
             [kw-req-id '()] [kw-req-key '()]
             [opt-id    '()] [opt-val '()]
             [kw-opt-id '()] [kw-opt-key '()] [kw-opt-val '()])
    (cond [(null? params)
           (list (reverse req-id)
                 (sort (map cons kw-req-key kw-req-id) (lambda (a b) (keyword-stx-<? (car a) (car b))))
                 (reverse (map cons opt-id opt-val))
                 (sort (map cons kw-opt-key (map cons kw-opt-id kw-opt-val)) (lambda (a b) (keyword-stx-<? (car a) (car b)))))]
          [(keyword? (syntax-e (car params)))
           (when (null? (cdr params))
             (error 'define/kw "invalid"))
           (syntax-case (cadr params) ()
             [(id val)
              (identifier? #'id)
              (loop (cddr params) req-id kw-req-id kw-req-key opt-id opt-val (cons #'id kw-opt-id) (cons (car params) kw-opt-key) (cons #'val kw-opt-val))]
             [id
              (identifier? #'id)
              (loop (cddr params) req-id (cons #'id kw-req-id) (cons (car params) kw-req-key) opt-id opt-val kw-opt-id kw-opt-key kw-opt-val)])]
          [(pair? (syntax-e (car params)))
           (syntax-case (car params) ()
             [(id val)
              (identifier? #'id)
              (loop (cdr params) req-id kw-req-id kw-req-key (cons #'id opt-id) (cons #'val opt-val) kw-opt-id kw-opt-key kw-opt-val)])]
          [(identifier? (car params))
           (loop (cdr params)  (cons (car params) req-id) kw-req-id kw-req-key opt-id opt-val kw-opt-id kw-opt-key kw-opt-val)]
          [else
           (error 'define/kw "invalid 2")])))

(define-for-syntax (args-parser stx name req-len kw-req-keys opt-vals kw-opt-keys kw-opt-vals rest-param)
  (lambda (args)
    (let loop ([args (stx->list args)]
               [non-keywords '()]
               [req-keys '()]
               [opt-keys '()]
               [req-len req-len]
               [kw-req-keys kw-req-keys]
               [opt-vals opt-vals]
               [kw-opt-keys+vals (map cons kw-opt-keys kw-opt-vals)])
      (cond [(null? args)
             (cond [(not (zero? req-len))
                    (error name "missing required arg, in ~A" args)]
                   [(not (null? kw-req-keys))
                    (error name "missing required key args, in ~A" args)]
                   [else
                    (append (map cdr (sort req-keys (lambda (a b) (keyword-<? (car a) (car b)))))
                            (map (lambda (p) (datum->syntax stx (cdr p))) (sort (append opt-keys kw-opt-keys+vals) (lambda (a b) (keyword-<? (car a) (car b)))))
                            (reverse non-keywords)
                            (map (lambda (v) (datum->syntax stx v)) opt-vals))])]
            [(keyword? (syntax-e (car args)))
             (when (null? (cdr args))
               (error name "no arg after keyword, in ~A" args))
             (cond [(member (syntax-e (car args)) kw-req-keys)
                    (let ([req-keys (cons (cons (syntax-e (car args)) (cadr args)) req-keys)]
                          [kw-req-keys (remove (syntax-e (car args)) kw-req-keys)])
                      (loop (cddr args) non-keywords req-keys opt-keys req-len kw-req-keys opt-vals kw-opt-keys+vals))]
                   [(memf (lambda (p) (equal? (car p) (syntax-e (car args)))) kw-opt-keys+vals)
                    (let ([opt-keys (cons (cons (syntax-e (car args)) (cadr args)) opt-keys)]
                          [kw-opt-keys+vals (remove (syntax-e (car args)) kw-opt-keys+vals (lambda (a b) (equal? a (car b))))])
                      (loop (cddr args) non-keywords req-keys opt-keys req-len kw-req-keys opt-vals kw-opt-keys+vals))]
                   [else (error name "unknown keyword ~A, in ~A" (syntax-e (car args)) args)])]
            [(and (zero? req-len) (null? opt-vals))
             (if (null? rest-param)
                 (error name "too many args ~A" args)
                 (loop (cdr args) (cons (car args) non-keywords) req-keys opt-keys req-len kw-req-keys opt-vals kw-opt-keys+vals))]
            [(zero? req-len)
             (loop (cdr args) (cons (car args) non-keywords) req-keys opt-keys req-len kw-req-keys (cdr opt-vals) kw-opt-keys+vals)]
            [else
             (loop (cdr args) (cons (car args) non-keywords) req-keys opt-keys (sub1 req-len) kw-req-keys opt-vals kw-opt-keys+vals)]))))

(define-syntax (define/kw stx)
  (syntax-case stx ()
    [(_ (name param ... . rest-param) . body)
     (or (identifier? #'rest-param) (stx-null? #'rest-param))
     (with-syntax ([((req-id ...)
                     ((kw-req-key . kw-req-id) ...)
                     ((opt-id . opt-val) ...)
                     ((kw-opt-key . (kw-opt-id . kw-opt-val)) ...))
                    (parse-params stx #'(param ...))])
       (with-syntax ([req-len    (length (stx->list #'(req-id ...)))]
                     [opt-len    (length (stx->list #'(opt-id ...)))])
         #'(begin
             (define (proc kw-req-id ... kw-opt-id ... req-id ... opt-id ... . rest-param) . body)
             (define-syntax (name stx)
               (define parse
                 (args-parser stx 'name req-len '(kw-req-key ...) '(opt-val ...) '(kw-opt-key ...) '(kw-opt-val ...) 'rest-param))
               (syntax-case stx ()
                 [(_ args (... ...))
                  (with-syntax ([(args (... ...)) (parse #'(args (... ...)))])
                    #'(proc args (... ...)))])))))]))

(provide define/kw)