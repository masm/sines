#lang scheme/base

(require
 scheme/base
 scheme/match
 "syntax.rkt"
 (prefix-in s: "syntax2.rkt")
 (prefix-in l: "../private/library.ss"))

(define (dispatch-lambda/arity->lambda op arity)
  (or (ormap (lambda (proc)
               (match proc
                      [(s:lambda args rest-arg body loc)
                       (and (or (= (length args) arity)
                                (and rest-arg
                                     (< (length args) arity)))
                            proc)]))
             (dispatch-lambda-stx-procs op))
      (error 'dispatch-lambda/arity->lambda "missing a proc of arity ~S" arity)))

(require "deserialize.rkt")

(define (app->let-node loc proc args)
  (define (fn ids rest-id args)
    (cond [(null? ids)
           (cond [rest-id (list (cons (list rest-id)
                                      (s:app (s:global-ref (sines-variable-id l:list)) args)))]
                 [(null? args) '()]
                 [else (error "too many arguments" loc)])]
          [(null? args) (error "too few arguments" loc)]
          [else (cons (cons (list (car ids)) (car args))
                      (fn (cdr ids) rest-id (cdr args)))]))
  (match proc
    [(s:lambda ids rest-id body)
     (let ([pairs (fn ids rest-id args)])
       (s:let-values (map car pairs) (map cdr pairs) body loc))]
    [(s:dispatch-lambda)
     (app->let-node loc (dispatch-lambda/arity->lambda proc (length args)) args)]
    [_ (error "contraction app->let-node: not matched" proc)]))

;; TODO: this should also rename the inner bindings, not just the outside lambda proc
(define (app->let-node/rename loc proc args)
  (define (fn ids rest-id args)
    (cond [(null? ids)
           (cond [rest-id (list (cons (list rest-id)
                                      (s:app (s:global-ref (sines-variable-id l:list)) args)))]
                 [(null? args) '()]
                 [else (error "too many arguments" loc)])]
          [(null? args) (error "too few arguments" loc)]
          [else (cons (cons (list (car ids)) (car args))
                      (fn (cdr ids) rest-id (cdr args)))]))
  (match proc
    [(s:lambda)
     (match (alpha-renamed-lexicals #:full? #f proc)
       [(s:lambda ids rest-id body)
        (let ([pairs (fn ids rest-id args)])
          (values (s:let-values (map car pairs) (map cdr pairs) body loc)))])]
    [(s:dispatch-lambda)
     (app->let-node loc (dispatch-lambda/arity->lambda proc (length args)) args)]
    [_ (error "contraction app->let-node: not matched" proc)]))

(provide dispatch-lambda/arity->lambda
         app->let-node app->let-node/rename)

(define next-in-seq
  (let ([c 0])
    (lambda ()
      (set! c (add1 c))
      c)))

(define (lexical-id-clone id)
  (make-id (id-name id) (next-in-seq)))

(define (new-lexical-id [name (gensym)])
  (make-id name (next-in-seq)))

(define (new-global-id [base "g"])
  (make-module-id (gensym base) #f))

(provide lexical-id-clone
         new-lexical-id
         new-global-id)

;;;

(define (alpha-renamed-lexicals node #:full? error-if-not-exist?)
  (let ([hash (make-hasheq)])
    (define maybe-renamed
      (if error-if-not-exist?
          (lambda (id)
            (cond [(hash-ref hash id (lambda () #f))]
                  [else (error 'alpha-renamed-lexicals "unknown lexical id ~A" id)]))
          (lambda (id)
            (cond [(hash-ref hash id (lambda () #f))]
                  [else id]))))
    (define (extend-hash! ids)
      (for-each (lambda (id)
                  (cond [(hash-ref hash id (lambda () #f)) => (lambda (_) (error 'alpha-renamed-lexicals "duplicate lexical id ~A" id))])
                  (hash-set! hash id (lexical-id-clone id)))
                ids))
    (let loop ([node node])
      (match node
        [(or (s:literal) (s:global-ref)) node]
        [(s:lexical-ref id loc)           (s:lexical-ref (maybe-renamed id) loc)]
        [(s:primapp transformer args loc) (s:primapp transformer (map loop args) loc)]
        [(s:app op args loc)              (s:app (loop op) (map loop args) loc)]
        [(s:begin body loc)               (s:begin (map loop body) loc)]
        [(s:define-values ids value loc)  (s:define-values ids (loop value) loc)]
        [(s:if test then else loc)        (s:if (loop test) (loop then) (loop else) loc)]
        [(s:lambda ids rest-id body loc)
         (extend-hash! (if rest-id (cons rest-id ids) ids))
         (s:lambda (map maybe-renamed ids)
                   (and rest-id (maybe-renamed rest-id))
                   (loop body) loc)]
        [(s:dispatch-lambda procs loc)    (s:dispatch-lambda (map loop procs) loc)]
        [(s:let-values ids vals body loc)
         (let ([vals (map loop vals)])
           (extend-hash! (apply append ids))
           (s:let-values (map (lambda (ids) (map maybe-renamed ids)) ids)
                         vals
                         (loop body)
                         loc))]
        [(s:fix ids procs body loc)
         (extend-hash! ids)
         (s:fix (map maybe-renamed ids) (map loop procs) (loop body) loc)]
        [(s:loop ids vals body loc)
         (let ([vals (map loop vals)])
           (extend-hash! ids)
           (s:loop (map maybe-renamed ids) vals (loop body) loc))]
        [(s:iterate args loc)             (s:iterate (map loop args) loc)]
        [(s:program body loc)             (s:program (map loop body) loc)]
        [_ (error 'alpha-renamed-lexicals "not matched" node)]))))

(provide alpha-renamed-lexicals)