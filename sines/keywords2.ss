#lang scheme

(require
 (for-syntax syntax/stx)
 "utils.ss"
 (for-syntax "utils.ss"))

;; Process options

(begin-for-syntax
  (define (process-options full-stx kw-spec)
    (let loop ((options (with-syntax (((_ . opts) kw-spec)) #'opts))
               (default #F) (predicate #F) (boolean #F))
      (syntax-case options ()
        ((:default expr . rest)
         (and (identifier?/name=? #':default ':default)
              (not default))
         (loop #'rest #'expr predicate boolean))
        ((:predicate expr . rest)
         (and (identifier?/name=? #':predicate ':predicate)
              (not predicate))
         (loop #'rest default #'expr boolean))
        ((:boolean . rest)
         (and (identifier?/name=? #':boolean ':boolean)
              (not boolean))
         (loop #'rest default predicate #T))
        (()
         (not (and boolean (or default predicate)))
         (values default predicate boolean))
        (_
         (syntax-violation #F "invalid options for keyword" full-stx kw-spec))))))

(define (process-options full-stx kw-spec)
  (let loop ((options (with-syntax (((_ . opts) kw-spec)) #'opts))
             (default #F) (predicate #F) (boolean #F))
    (syntax-case options ()
      ((:default expr . rest)
       (and (identifier?/name=? #':default ':default)
            (not default))
       (loop #'rest #'expr predicate boolean))
      ((:predicate expr . rest)
       (and (identifier?/name=? #':predicate ':predicate)
            (not predicate))
       (loop #'rest default #'expr boolean))
      ((:boolean . rest)
       (and (identifier?/name=? #':boolean ':boolean)
            (not boolean))
       (loop #'rest default predicate #T))
      (()
       (not (and boolean (or default predicate)))
       (values default predicate boolean))
      (_
       (syntax-violation #F "invalid options for keyword" full-stx kw-spec)))))

;; Parser

(define (AV who msg kw . more)
  (raise
   (apply condition
          (make-assertion-violation)
          (make-who-condition who)
          (make-message-condition msg)
          (make-keyword-condition kw)
          more)))

(define (missing-value--default who kw)
  (AV who "keyword missing value" kw))

(define (missing-keyword--default who kw)
  (AV who "missing required keyword" kw))

(define (predicate-false--default who kw pred-expr value)
  (AV who "keyword predicate false" kw
      (make-predicate-expression-condition pred-expr)
      (make-irritants-condition (list value))))

(define not-given (list #T))  ;; unique object
(define (not-given? x) (eq? x not-given))

(define-syntax (keywords-parser--meta stx)
  (define (gen-stx who kw=? missing-value missing-keyword predicate-false
                   kw-values process-input-list car-id cdr-id additional)
    (lambda (kw-spec index)
      (with-syntax (((kw-id . _) kw-spec)
                    (v (gen-temp)))
        (define (gen-clause/val)
          #`((#,kw=? #,car-id 'kw-id)
             (if (pair? #,cdr-id)
                 (vector-set! #,kw-values #,index (car #,cdr-id))
                 (#,missing-value '#,who 'kw-id))
             (#,process-input-list (cdr #,cdr-id) #,additional)))
        (define (gen-test/val true false)
          #`(let ((v (vector-ref #,kw-values #,index)))
              (if (not-given? v) #,false #,true)))
        (define (gen-pred/val pred)
          #`(if (#,pred v)
                v
                (#,predicate-false '#,who 'kw-id '#,pred v)))
        (define (gen-missing)
          #`(#,missing-keyword '#,who 'kw-id))
        (define (gen-clause/bool)
          #`((#,kw=? #,car-id 'kw-id)
             (vector-set! #,kw-values #,index #T)
             (#,process-input-list #,cdr-id #,additional)))
        (define (gen-test/bool)
          #`(not (not-given? (vector-ref #,kw-values #,index))))
        (let-values ([(default predicate boolean) (process-options stx kw-spec)])
          (cond [(and default predicate)
                 (list (gen-clause/val)
                       (gen-test/val (gen-pred/val predicate) default))]
                [default
                  (list (gen-clause/val)
                        (gen-test/val #'v default))]
                [predicate
                 (list (gen-clause/val)
                       (gen-test/val (gen-pred/val predicate) (gen-missing)))]
                [boolean
                 (list (gen-clause/bool)
                       (gen-test/bool))]
                [else
                 (list (gen-clause/val)
                       (gen-test/val #'v (gen-missing)))])))))
  (syntax-case stx ()
    [(_ who kw=? missing-value missing-keyword predicate-false
        (kw-id options ...) ...)
     (andmap identifier?
             (stx->list #'(kw=? missing-value missing-keyword predicate-false kw-id ...)))
     (with-syntax ([num (length (stx->list #'(kw-id ...)))]
                   [(kw-values process-input-list car-id cdr-id additional)
                    (generate-temporaries '(1 2 3 4 5))])
       (with-syntax ([((cond-clause value-expr) ...)
                      (map (gen-stx #'who #'kw=?
                                    #'missing-value #'missing-keyword #'predicate-false
                                    #'kw-values #'process-input-list
                                    #'car-id #'cdr-id #'additional)
                           (stx->list #'((kw-id options ...) ...))
                           (enumerate (stx->list #'(kw-id ...))))])
         #'(lambda (input-list)
             (let ([kw-values (make-vector num not-given)])
               (let process-input-list ([l (stx->list input-list)]
                                        [additional '()])
                 (cond [(pair? l)
                        (let ([car-id (car l)] [cdr-id (cdr l)])
                          (cond cond-clause ...
                                (else (process-input-list cdr-id
                                                          (cons car-id additional)))))]
                       [(null? l)
                        (define kw-id value-expr) ...
                        (values kw-id ... (reverse additional))]
                       [else
                        (assertion-violation 'who "not a proper list" input-list)]))))))]))

;; XXX

(define (unexpected-additionals--default who addts)
  (apply assertion-violation who "unexpected additional arguments" addts))

(define clause-failed (list #T))  ;; unique object
(define (clause-failed? x) (eq? x clause-failed))

(define (missing-value--clause-failed who kw-id)
  (raise clause-failed))

(define (missing-keyword--clause-failed who kw-id)
  (raise clause-failed))

(define (predicate-false--clause-failed who kw-id pred-expr value)
  (raise clause-failed))

(define (unexpected-additionals--clause-failed who addts)
  (raise clause-failed))

(define (make-case-lambda/kw who procs)
  (lambda args
    (let ((len (length args)))
      (let try-next ((procs procs))
        (if (pair? procs)
            (if (>= len (caar procs))
                ((call/cc
                  (lambda (k)
                    (with-exception-handler
                     (lambda (ex)
                       (if (clause-failed? ex)
                           (k (lambda () (try-next (cdr procs))))
                           (reraise ex)))
                     (lambda ()
                       (call-with-values (lambda () (apply (cdar procs) args))
                         (lambda vals (apply values vals))))))))
                (try-next (cdr procs)))
            (apply assertion-violation who "no clause matches arguments" args))))))

(provide process-options
         keywords-parser--meta
         unexpected-additionals--default)
