#lang scheme/base

(require
 scheme/base
 scheme/contract
 scheme/list
 scheme/match
 "../primitives.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../store.rkt"
 "../utils.rkt"
 "../backend.rkt")

;; A better option would be to inline procedures first:
;; We put all the procedures in an hash and start to inline them recursively, on use
;; by other procedures. Then we inline the remaining expressions

(define (transform node)
  (auto-inlining node (definitions-hash node) (make-hash) '()))

(define (definitions-hash node)
  (match node
    [(s:program body)
     (let ([hash (make-hash)])
       (for-each (lambda (node)
                   (match node
                     [(s:define-values (list id) value)
                      (hash-set! hash id value)]
                     [_ (void)]))
                 body)
       hash)]
    [_ (error "auto-inlining hash-of-definitions: not matched" node)]))

(define (auto-inlining node untransformed transformed recursive)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref)) node]
    [(s:primapp op args loc) (s:primapp op (list-auto-inlining args untransformed transformed recursive) loc)]
    [(struct app-stx [loc op args])
     (let ([op (auto-inlining op untransformed transformed recursive)]
           [args (list-auto-inlining args untransformed transformed recursive)])
       (let ([new-app (make-app-stx loc op args)])
         (match op
           [(struct global-ref-stx [loc id])
            (cond [(hash-ref transformed id (lambda () #f)) => (lambda (pair) ((cdr pair) new-app))]
                  [(and (not (member id recursive))
                        (hash-ref untransformed id (lambda () #f))) ;; No error if id does not exist because it can be part of multiple-value definition
                   => (lambda (value)
                        (let ([new-value (auto-inlining value untransformed transformed (cons id recursive))])
                          (let ([inliner (make-inliner id new-value)])
                            (hash-set! transformed id (cons new-value inliner))
                            (inliner new-app))))]
                  [else new-app])]
           [_ new-app])))]
    [(struct begin-stx [loc body]) (make-begin-stx loc (list-auto-inlining body untransformed transformed recursive))]
    [(struct define-values-stx [loc ids value])
     (make-define-values-stx loc ids
                             (cond [(and (not (null? ids))
                                         (null? (cdr ids))
                                         (hash-ref transformed (car ids) (lambda () #f))) => car]
                                   [else (auto-inlining value untransformed transformed (cons id recursive))]))]
    [(struct if-stx [loc test then else])
     (make-if-stx loc
                  (auto-inlining test untransformed transformed recursive)
                  (auto-inlining then untransformed transformed recursive)
                  (auto-inlining else untransformed transformed recursive))]
    [(struct lambda-stx [loc ids rest-id body]) (make-lambda-stx loc ids rest-id (auto-inlining body untransformed transformed recursive))]
    [(struct dispatch-lambda-stx [loc procs]) (make-dispatch-lambda-stx loc (list-auto-inlining procs untransformed transformed recursive))]
    [(struct let-values-stx [loc ids values body])
     (make-let-values-stx loc ids
                          (list-auto-inlining values untransformed transformed recursive)
                          (auto-inlining body untransformed transformed recursive))]
    [(struct fix-stx [loc ids procs body])
     (make-fix-stx loc ids
                   (list-auto-inlining procs untransformed transformed recursive)
                   (auto-inlining body untransformed transformed recursive))]
    [(struct wcm-stx [loc key value expr])
     (make-wcm-stx loc
                  (auto-inlining key untransformed transformed recursive)
                  (auto-inlining value untransformed transformed recursive)
                  (auto-inlining expr untransformed transformed recursive))]
    [(s:loop) node]
    [(struct program-stx [loc body]) (make-program-stx loc (list-auto-inlining body untransformed transformed recursive))]
    [_ (error "auto-inlining: not matched" node)]))

(define (list-auto-inlining nodes untransformed transformed recursive)
  (map (lambda (node)
         (auto-inlining node untransformed transformed recursive))
       nodes))

(define always-inline-procedure-names
  (make-hash (map (lambda (var) (cons var #t))
                  (list
                   ;; (sines-variable-id l:+)
                   ;; (sines-variable-id l:-)
                   ;; (sines-variable-id l:*)
                   ;; (sines-variable-id l:/)
                   ;; (sines-variable-id l:=)
                   ;; (sines-variable-id l:<)
                   ;; (sines-variable-id l:>)
                   ;; (sines-variable-id l:<=)
                   ;; (sines-variable-id l:>=)
                   ;; (sines-variable-id l:string=?)
                   ;; (sines-variable-id l:string<?)
                   ;; (sines-variable-id l:string>?)
                   ;; (sines-variable-id l:string<=?)
                   ;; (sines-variable-id l:string>=?)
                   ))))

(define (make-inliner id node)
  (cond [(and (lambda-stx? node)
              (or (hash-has-key? always-inline-procedure-names id)
                  (simple? (lambda-stx-body node))))
         (lambda (app)
           (match app
             [(s:app _ args loc)
              (app->let-node/rename loc node args)]))]
        [(and (dispatch-lambda-stx? node)
              (hash-has-key? always-inline-procedure-names id))
         (lambda (app)
           (match app
             [(s:app _ args loc)
              (let ([proc (dispatch-lambda/arity->lambda node (length args))])
                (if (simple? (lambda-stx-body proc))
                    (app->let-node/rename loc proc args)
                    app))]))]
        [else (lambda (x) x)]))

(define simple-procedure-names
  (make-hash (map (lambda (var) (cons var #t))
                  (list
                   ;; (sines-variable-id l:apply)
                   ))))

(define (simple? node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref) (s:loop)) #t]
    [(or (s:app) (s:lambda) (s:dispatch-lambda) (s:wcm)) #f]
    [(s:app (s:global-ref id) args) (and (hash-has-key? simple-procedure-names id) (andmap simple? args))]
    [(s:primapp _ args)             (andmap simple? args)]
    [(s:begin body)                 (andmap simple? body)]
    [(s:if test then else)          (and (simple? test) (simple? then) (simple? else))]
    [(s:let-values _ values body)   (and (andmap simple? values) (simple? body))]
    [(s:fix _ procs body)           (and (andmap simple? procs) (simple? body))]
    [_ (error 'auto-inlining "simple?: not matched" node)]))

(provide/contract
 [transform (stx? . -> . stx?)])
