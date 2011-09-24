#lang scheme

(require syntax/id-table)

(require "store.rkt")

(define-struct stx                     (loc)                   #:prefab)

(define-struct (module-stx stx)        (body)                  #:prefab)
(define-struct (primapp-stx stx)       (transformer args)      #:prefab)
(define-struct (app-stx stx)           (op args)               #:prefab)
(define-struct (lambda-stx stx)        (args rest-arg body)    #:prefab)
(define-struct (begin-stx stx)         (body)                  #:prefab)
(define-struct (case-lambda-stx stx)   (clauses)               #:prefab)
(define-struct (case-lambda-clause-stx stx) (args rest-arg body) #:prefab)
(define-struct (define-values-stx stx) (variables value)       #:prefab)
(define-struct (if-stx stx)            (test then else)        #:prefab)
(define-struct (let-values-stx stx)    (variables values body) #:prefab)
(define-struct (letrec-values-stx stx) (variables values body) #:prefab)
(define-struct (literal-stx stx)       (value)                 #:prefab)
(define-struct (lexical-ref-stx stx)   (id)                    #:prefab)
(define-struct (global-ref-stx stx)    (id)                    #:prefab)
(define-struct (set!-stx stx)          (variable value)        #:prefab)
(define-struct (wcm-stx stx)           (key value expr)        #:prefab)

(define-struct (program-stx stx)       (body)                  #:prefab)

(define-struct (dispatch-lambda-stx stx) (procs)               #:prefab)
(define-struct (named-lambda-stx stx)  (name args rest-arg body) #:prefab )
(define-struct (fix-stx stx)           (ids procs body)        #:prefab)
(define-struct (begin/var-stx stx)     (ids body)              #:prefab)
(define-struct (program/var-stx stx)   (global-ids lexical-ids body) #:prefab)
(define-struct (begin/const/var-stx stx) (const-ids const-values ids body) #:prefab)
(define-struct (program/const/var-stx stx) (const-global-ids
                                            const-global-values
                                            const-lexical-ids
                                            const-lexical-values
                                            global-ids lexical-ids body) #:prefab)

(define-struct (loop-stx stx)          (ids values body)       #:prefab)
(define-struct (iterate-stx stx)       (args)                  #:prefab)

(define-struct (blocks-stx stx)        (ids blocks)            #:prefab)
(define-struct (block-stx stx)         (frames body)           #:prefab)

(define-struct (global-init-stx stx)   (id value)              #:prefab)
(define-struct (lexical-init-stx stx)  (id value)              #:prefab)

(define-struct (tail-stx stx)          (app)                   #:prefab)
(define-struct (non-tail-stx stx)      (app)                   #:prefab)

(define-struct (if-true-stx stx)       (test then)             #:prefab)
(define-struct (if-false-stx stx)      (test then)             #:prefab)

(define-struct id (name binding) #:prefab)

(define-struct module-id (name source)
  #:transparent ;; #:prefab
  #:property prop:equal+hash (list (lambda (a b equal?-recur)
                                     (and (equal?-recur (module-id-name a) (module-id-name b))
                                          (equal?-recur (module-id-source a) (module-id-source b))))
                                   (lambda (a hash-recur)
                                     (+ (hash-recur (module-id-name a))
                                        (* 3 (hash-recur (module-id-source a)))))
                                   (lambda (a hash2-recur)
                                     (+ (hash2-recur (module-id-name a))
                                        (hash2-recur (module-id-source a)))))
  )

(define-struct loc (source position line column span) #:prefab)

(define (loc->string a-loc)
  (format "offset=~a line=~a column=~a span=~a id=~s"
          (loc-position a-loc)
          (loc-line a-loc)
          (loc-column a-loc)
          (loc-span a-loc)
          (loc-source a-loc)))

(provide/contract
 [struct program-stx   ([loc (or/c #f loc?)]
                        [body (listof stx?)])]
 [struct module-stx    ([loc (or/c #f loc?)]
                        [body (listof stx?)])]
 [struct primapp-stx   ([loc (or/c #f loc?)]
                        [transformer any/c]
                        [args (listof stx?)])]
 [struct app-stx       ([loc (or/c #f loc?)]
                        [op stx?]
                        [args (listof stx?)])]
 [struct lambda-stx    ([loc (or/c #f loc?)]
                        [args (listof id?)]
                        [rest-arg (or/c #f id?)]
                        [body stx?])]
 [struct begin-stx     ([loc (or/c #f loc?)]
                        [body (listof stx?)])]
 [struct case-lambda-stx ([loc (or/c #f loc?)]
                          [clauses (listof case-lambda-clause-stx?)])]
 [struct case-lambda-clause-stx ([loc (or/c #f loc?)]
                                 [args (listof id?)]
                                 [rest-arg (or/c #f id?)]
                                 [body stx?])]
 [struct dispatch-lambda-stx ([loc (or/c #f loc?)]
                              [procs (listof lambda-stx?)])]
 [struct define-values-stx ([loc (or/c #f loc?)]
                            [variables (listof any/c)]
                            [value stx?])]
 [struct if-stx ([loc (or/c #f loc?)]
                 [test stx?]
                 [then stx?]
                 [else stx?])]
 [struct if-false-stx ([loc (or/c #f loc?)]
                       [test stx?]
                       [then stx?])]
 [struct if-true-stx ([loc (or/c #f loc?)]
                      [test stx?]
                      [then stx?])]
 [struct let-values-stx ([loc (or/c #f loc?)]
                         [variables (listof (listof id?))]
                         [values (listof stx?)]
                         [body stx?])]
 [struct letrec-values-stx ([loc (or/c #f loc?)]
                            [variables (listof (listof id?))]
                            [values (listof stx?)]
                            [body stx?])]
 [struct fix-stx ([loc (or/c #f loc?)]
                  [ids (listof id?)]
                  [procs (listof stx?)]
                  [body stx?])]
 [struct literal-stx ([loc (or/c #f loc?)]
                      [value any/c])]
 [struct lexical-ref-stx ([loc (or/c #f loc?)]
                          [id id?])]
 [struct global-ref-stx ([loc (or/c #f loc?)]
                         [id module-id?])]
 [struct set!-stx ([loc (or/c #f loc?)]
                   [variable (or/c lexical-ref-stx? global-ref-stx?)]
                   [value stx?])]
 [struct wcm-stx ([loc (or/c #f loc?)]
                  [key stx?]
                  [value stx?]
                  [expr stx?])]

 [struct program/var-stx ([loc (or/c #f loc?)]
                          [global-ids (listof module-id?)]
                          [lexical-ids (listof id?)]
                          [body (listof stx?)])]
 [struct begin/var-stx ([loc (or/c #f loc?)]
                        [ids (listof id?)]
                        [body (listof stx?)])]
 [struct program/const/var-stx ([loc (or/c #f loc?)]
                                [const-global-ids (listof module-id?)]
                                [const-global-values (listof stx?)]
                                [const-lexical-ids (listof id?)]
                                [const-lexical-values (listof stx?)]
                                [global-ids (listof module-id?)]
                                [lexical-ids (listof id?)]
                                [body (listof stx?)])]
 [struct begin/const/var-stx ([loc (or/c #f loc?)]
                              [const-ids (listof id?)]
                              [const-values (listof stx?)]
                              [ids (listof id?)]
                              [body (listof stx?)])]
 [struct loop-stx ([loc (or/c #f loc?)]
                   [ids (listof id?)]
                   [values (listof stx?)]
                   [body stx?])]
 [struct iterate-stx ([loc (or/c #f loc?)]
                      [args (listof stx?)])]
 [struct blocks-stx ([loc (or/c #f loc?)]
                     [ids (listof id?)]
                     [blocks (listof stx?)])]
 [struct block-stx ([loc (or/c #f loc?)]
                    [frames (listof (listof id?))]
                    [body (listof stx?)])]
 [struct lexical-init-stx ([loc (or/c #f loc?)]
                           [id id?]
                           [value stx?])]
 [struct global-init-stx ([loc (or/c #f loc?)]
                          [id module-id?]
                          [value stx?])]
 [struct non-tail-stx ([loc (or/c #f loc?)]
                       [app stx?])]
 [struct tail-stx ([loc (or/c #f loc?)]
                   [app stx?])]

 ;; [struct id ([name (or/c #f symbol?)]
 ;;             [binding (or/c #f number? string?)])]
 [struct loc ([source (or/c #f any/c)]
              [position (or/c #f integer?)]
              [line (or/c #f integer?)]
              [column (or/c #f integer?)]
              [span (or/c #f integer?)])]
 [stx? (any/c . -> . boolean?)]
 [stx-loc (stx? . -> . any)]
 [loc->string (loc? . -> . string?)])

(provide (struct-out id)
         (struct-out module-id))

(define (map-node proc node)
  (define (recur node)
    (proc node recur proceed))
  (define (proceed node)
    (generic-map-node node recur))
  (recur node))

(define (generic-map-node node recur)
  (match node
    [(struct literal-stx _)
     node]
    [(struct lexical-ref-stx _)
     node]
    [(struct global-ref-stx _)
     node]
    [(struct primapp-stx [loc transformer args])
     (make-primapp-stx loc transformer (map recur args))]
    [(struct app-stx [loc op args])
     (make-app-stx loc (recur op) (map recur args))]
    [(struct begin-stx [loc body])
     (make-begin-stx loc (map recur body))]
    [(struct case-lambda-stx [loc clauses])
     (make-case-lambda-stx loc
                           (map (lambda (clause)
                                  (match clause
                                    [(struct case-lambda-clause-stx [loc args rest-arg body])
                                     (make-case-lambda-clause-stx loc args rest-arg (recur body))]))
                                clauses))]
    [(struct dispatch-lambda-stx [loc procs])
     (make-dispatch-lambda-stx loc (map recur procs))]
    [(struct define-values-stx [loc vars value])
     (make-define-values-stx loc vars (recur value))]
    [(struct if-stx [loc test then else])
     (make-if-stx loc (recur test) (recur then) (recur else))]
    [(struct lambda-stx [loc params rest-param body])
     (make-lambda-stx loc params rest-param (recur body))]
    [(struct let-values-stx [loc ids values body])
     (make-let-values-stx loc ids (map recur values) (recur body))]
    [(struct letrec-values-stx [loc ids values body])
     (make-letrec-values-stx loc ids (map recur values) (recur body))]
    [(struct set!-stx [loc var value])
     (make-set!-stx loc (recur var) (recur value))]
    [(struct program-stx [loc body])
     (make-program-stx loc (map recur body))]
    [(struct begin/var-stx [loc ids body])
     (make-begin/var-stx loc ids (map recur body))]
    [(struct wcm-stx [loc key value expr])
     (make-wcm-stx loc (recur key) (recur value) (recur expr))]
    [(struct program/var-stx [loc ids ids2 body])
     (make-program/var-stx loc ids ids2 (map recur body))]
    [_ (error "generic-map-node: not matched" node)]))

(provide map-node)

(define-struct sines-variable (id code))

(provide (struct-out sines-variable))
