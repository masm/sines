#lang at-exp racket/base

(require
 racket/base
 racket/contract
 racket/list
 racket/match
 "../../externals/javascript/ast.rkt"

 "../backend.rkt"
 (prefix-in c: "../ecmascript.rkt")
 "../primitives.rkt"
 "../snippets.rkt"
 "../store.rkt"
 "../syntax.rkt"
 (prefix-in s: "../syntax2.rkt")
 "../utils.ss"
 (prefix-in l: "../../private/library.ss"))

(define globals-hash (make-parameter #f))

(define (variable-name id)
  (hash-ref (globals-hash) id))

(define (value-name value)
  (variable-name (sines-variable-id value)))

(define (global-ids-from-node node)
  (match node
    [(s:program/const/var const-global-ids _ _ _ global-ids _ body)
     (append const-global-ids global-ids)]
    [_ '()]))

(define lexicals-hash (make-parameter #f))

(define (lexical-id-name id)
  (hash-ref (lexicals-hash) id))

(define (lexical-ids-from-node node)
  (match node
    [(or (s:literal) (s:lexical-ref) (s:global-ref))
     '()]
    [(s:primapp primitive args)
     (append-map lexical-ids-from-node args)]
    [(s:app op args)
     (append (lexical-ids-from-node op)
             (append-map lexical-ids-from-node args))]
    [(s:tail app)
     (lexical-ids-from-node app)]
    [(s:non-tail app)
     (lexical-ids-from-node app)]
    [(s:blocks ids blocks)
     (append-map lexical-ids-from-node blocks)]
    [(s:block frames body)
     (append-map lexical-ids-from-node body)]
    [(s:begin body)
     (append-map lexical-ids-from-node body)]
    [(s:begin/const/var const-ids const-values ids body)
     (append const-ids ids
             (append-map lexical-ids-from-node const-values)
             (append-map lexical-ids-from-node body))]
    [(s:if-true test then)
     (append (lexical-ids-from-node test)
             (lexical-ids-from-node then))]
    [(s:if test then else)
     (append (lexical-ids-from-node test)
             (lexical-ids-from-node then)
             (lexical-ids-from-node else))]
    [(s:lambda ids rest-id body)
     (append (if rest-id (cons rest-id ids) ids)
             (lexical-ids-from-node body))]
    [(s:dispatch-lambda procs)
     (append-map lexical-ids-from-node procs)]
    [(s:loop ids values body)
     (append (append-map lexical-ids-from-node values)
             (lexical-ids-from-node body))]
    [(s:iterate args)
     (append-map lexical-ids-from-node args)]
    [(s:lexical-init _ value)
     (lexical-ids-from-node value)]
    [(s:global-init _ value)
     (lexical-ids-from-node value)]
    [(s:program/const/var _ const-global-values
                          const-lexical-ids const-lexical-values
                          _ lexical-ids body)
     (append const-lexical-ids lexical-ids
             (append-map lexical-ids-from-node const-global-values)
             (append-map lexical-ids-from-node const-lexical-values)
             (append-map lexical-ids-from-node body))]
    [_ (error 'javascriptify "lexical-ids-from-node: not matched, in ~A" node)]))

;;;; Actual translator

(define transform
  (let ([symvec
         '#(a b c d e f g h i j k l m n o p q r s t u v w x y z
            a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af ag ah ai aj ak al am an ao ap
            b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf bg bh bi bj bk bl bm bn bo bp
            c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf cg ch ci cj ck cl cm cn co cp
            d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df dg dh di dj dk dl dm dn    dp
            e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 ea eb ec ed ee ef eg eh ei ej ek el em en eo ep
            f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 fa fb fc fd fe ff fg fh fi fj fk fl fm fn fo fp
            g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 ga gb gc gd ge gf gg gh gi gj gk gl gm gn go gp
            h0 h1 h2 h3 h4 h5 h6 h7 h8 h9 ha hb hc hd he hf hg hh hi hj hk hl hm hn ho hp
            i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 ia ib ic id ie    ig ih ii ij ik il im    io ip
            j0 j1 j2 j3 j4 j5 j6 j7 j8 j9 ja jb jc jd je jf jg jh ji jj jk jl jm jn jo jp
            k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 ka kb kc kd ke kf kg kh ki kj kk kl km kn ko kp
            l0 l1 l2 l3 l4 l5 l6 l7 l8 l9 la lb lc ld le lf lg lh li lj lk ll lm ln lo lp
            m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 ma mb mc md me mf mg mh mi mj mk ml mm mn mo mp
            n0 n1 n2 n3 n4 n5 n6 n7 n8 n9 na nb nc nd ne nf ng nh ni nj nk nl nm nn no np
            o0 o1 o2 o3 o4 o5 o6 o7 o8 o9 oa ob oc od oe of og oh oi oj ok ol om on oo op
            )])
    (let ([n (vector-length symvec)])
      (lambda (node)
        (let ([i -1])
          (define next-symbol
            (lambda ()
              (set! i (add1 i))
              (if (< i n)
                  (vector-ref symvec i)
                  (string->symbol (string-append "q" (number->string (- i n) 16))))))

          (parameterize ([globals-hash (make-immutable-hash (map (lambda (id)
                                                                   (cons id (next-symbol)))
                                                                 (global-ids-from-node node)))]
                         [lexicals-hash (make-immutable-hash (map (lambda (id)
                                                                    (cons id (next-symbol)))
                                                                  (lexical-ids-from-node node)))])
            (stx->javascript node)))))))

(define (stx->javascript stx)
  (match stx
    [(s:literal d loc)               (literal->javascript-expression d loc)]
    [(s:lexical-ref id loc)          (stx->javascript/lexical-ref id loc)]
    [(s:global-ref id loc)           (stx->javascript/global-ref id loc)]
    [(s:primapp primitive args loc)  (apply (sines-primitive-transformer primitive) stx->javascript-expr loc args)]
    [(s:app op args loc)             (apply c:call (stx->javascript-expr op) (map stx->javascript-expr args))]
    [(s:tail app)                    (stx->javascript/tail app)]
    [(s:non-tail app)                (stx->javascript/non-tail app)]
    [(s:if-true test then loc)       (stx->javascript/if-true test then loc)]
    [(s:if test then else loc)       (c:if (c:!== (stx->javascript-expr test) #f) (stx->javascript then) (stx->javascript else) #:loc loc)]
    [(s:begin body)                  (apply c:block (map stx->javascript body))]
    [(s:begin/const/var const-ids const-values ids body) (stx->javascript/begin/const/var const-ids const-values ids body)]
    [(s:lambda ids rest-id body)     (stx->javascript/lambda ids rest-id body)]
    [(s:dispatch-lambda clauses loc) (stx->javascript/dispatch-lambda clauses loc)]
    [(s:lexical-init id value loc)   (c:= (c:var-ref (make-Identifier #f (lexical-id-name id)) #:loc loc) (stx->javascript-expr value) #:loc loc)]
    [(s:global-init id value loc)    (c:= (c:var-ref (make-Identifier #f (variable-name id)) #:loc loc) (stx->javascript-expr value) #:loc loc)]
    [(s:loop ids values body loc)    (stx->javascript/loop ids values body loc)]
    ;; [(s:iterate args loc)            (stx->javascript/iterate args loc)] ;; this is always in tail position
    [(s:blocks ids blocks)           (stx->javascript/blocks  ids blocks)]
    [(s:program/const/var const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body loc)
     (stx->javascript/program/const/var const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body loc)]
    [_ (error 'javascriptify "stx->javascript: not matched" stx)]))

(define (stx->javascript-expr stx)
  (match stx
    [(s:literal d loc)               (literal->javascript-expression d loc)]
    [(s:lexical-ref id loc)          (stx->javascript/lexical-ref id loc)]
    [(s:global-ref id loc)           (stx->javascript/global-ref id loc)]
    [(s:primapp primitive args loc)  (apply (sines-primitive-transformer primitive) stx->javascript-expr loc args)]
    [(s:app op args loc)             (apply c:call (stx->javascript-expr op) (map stx->javascript-expr args))]
    [(s:if test then else loc)       (c:if-e (c:!== (stx->javascript-expr test) #f) (stx->javascript-expr then) (stx->javascript-expr else) #:loc loc)]
    [(s:begin body)                  (apply c:comma (map stx->javascript-expr body))]
    [(s:lambda ids rest-id body)     (stx->javascript/lambda ids rest-id body)]
    [(s:dispatch-lambda clauses loc) (stx->javascript/dispatch-lambda clauses loc)]
    [(s:loop ids vals body loc)      (c:call (c:fn '() (stx->javascript/loop ids vals body loc)))]
    [(s:lexical-init id value loc)   (c:= (c:var-ref (make-Identifier #f (lexical-id-name id)) #:loc loc) (stx->javascript-expr value) #:loc loc)]
    [_ (error "javascriptify-expr: not matched" stx)]))

(define (stx->javascript/lexical-ref id loc)
  ;; This HACK should be temporary; use lexical-id-name
  (c:var-ref (make-Identifier #f (hash-ref (lexicals-hash) id (lambda () (id-name id)))) #:loc loc))

(define (stx->javascript/global-ref id loc)
  (c:var-ref (make-Identifier #f (variable-name id)) #:loc loc))

(define (stx->javascript/tail app)
  (match app
    [(s:app op args)
     (tail-call-trampoline (stx->javascript op)
                           (map stx->javascript args)
                           (c:var-ref (value-name l:tail-object))
                           (c:var-ref (value-name l:tail-trampoline))
                           (c:var-ref (value-name l:tail-trampoline-restart))
                           'zr 'zs)]
    [(s:if test then else loc)    (c:return (stx->javascript-expr/if test then else loc))]
    [(s:loop ids values body loc) (stx->javascript/loop ids values body loc)] ; the return is inserted inside the loop
    [(s:iterate args loc)         (stx->javascript/iterate args loc)]         ; we do not return, but iterate
    [_ (c:return (stx->javascript app))]))

(define (stx->javascript/non-tail app)
  (match app
    [(s:if test then else loc)    (c:expr (stx->javascript-expr/if test then else loc))]
    [(s:loop ids values body loc) (c:call (c:fn '() (stx->javascript/loop ids values body loc)))] ; non-tail expression position: use function wrapper
    [_ (c:expr (stx->javascript app))]))

(define (stx->javascript-expr/if test then else loc)
  (c:if-e (c:!== (stx->javascript test) #f) (stx->javascript-expr then) (stx->javascript-expr else) #:loc loc))

(define loop-ids (make-parameter #f))

(define (stx->javascript/loop ids values body loc)
  (apply c:block
         (append (map (lambda (id value) (c:expr (c:= (c:var-ref (make-Identifier #f (lexical-id-name id))) (stx->javascript value))))
                      ids values)
                 (list (c:while #:loc loc
                                #t
                                (parameterize ([loop-ids ids])
                                  (stx->javascript body)))))))


(define (stx->javascript/iterate args loc)
  (define (take-one-unused ids args)
    (let loop ([unseen-ids ids]
               [unseen-args args]
               [seen-ids '()]
               [seen-args '()])
      (if (null? unseen-ids)
          (values #f #f ids args)
          (let ([this-id (car unseen-ids)]
                [this-arg (car unseen-args)]
                [other-ids (append seen-ids (cdr unseen-ids))]
                [other-args (append seen-args (cdr unseen-args))])
            (if (not-used-in this-id other-args)
                (values this-id this-arg other-ids other-args)
                (loop (cdr unseen-ids) (cdr unseen-args) (cons this-id seen-ids) (cons this-arg seen-args)))))))
  (define (not-used-in id values)
    (not (ormap (lambda (v) (used-in id v)) values)))
  (define (used-in id value)
    (let loop ([node value])
      (match node
        [(or (s:literal) (s:global-ref)) #f]
        [(s:lexical-ref _id)       (equal? id _id)]
        [(s:primapp _ args)        (ormap loop args)]
        [(s:begin body)            (ormap loop body)]
        [(s:lexical-init _id value)
         (when (equal? id _id)
           (error 'used-in "unexpected"))
         (loop value)])))
  (define (generate-tmp-expr-and-replace id vals)
    (let ([new-id (new-lexical-id)])
      (values (c:var-decl (cons (id-name new-id) (c:var-ref (make-Identifier #f (lexical-id-name id)))))
              (map (lambda (value) (replace-use-of-id id new-id value)) vals))))
  (define (replace-use-of-id old-id new-id value)
    (let loop ([node value])
      (match node
        [(or (s:literal) (s:global-ref)) node]
        [(s:lexical-ref id loc)        (if (equal? old-id id) (s:lexical-ref new-id loc) node)]
        [(s:primapp prim args loc)     (s:primapp prim (map loop args) loc)]
        [(s:begin body loc)            (s:begin (map loop body) loc)]
        [(s:lexical-init id value loc)
         (when (equal? old-id id)
           (error 'replace-use-of-id "unexpected"))
         (s:lexical-init id (loop value) loc)])))
  (let loop ([ids (loop-ids)]
             [args args]
             [acc '()])
    (cond [(null? ids)
           (apply c:block (reverse acc))]
          [else
           (let-values ([(id value ids args) (take-one-unused ids args)])
             (if (not id)
                 (let-values ([(tmp-expr args) (generate-tmp-expr-and-replace (car ids) args)])
                   (loop ids args (cons tmp-expr acc)))
                 (loop ids args (cons (c:expr (c:= (c:var-ref (make-Identifier #f (lexical-id-name id))) (stx->javascript value)))
                                      acc))))])))

(define (stx->javascript/if-true test then loc)
  (match then
    [(s:if-true)
     (match (stx->javascript then)
       [(struct IfStatement [_ test2 then2 else2])
        (c:if (c:&& (c:!== (stx->javascript test) #f)
                    test2)
              then2
              else2
              #:loc loc)]
       [_ (error "assertion error")])]
    [_ (c:if (c:!== (stx->javascript test) #f)
             (stx->javascript then)
             (c:empty)
             #:loc loc)]))

(define (stx->javascript/begin/const/var const-ids const-values ids body)
  (apply c:block
         (append (map (lambda (id value)
                        (init->javascript-statement (lexical-id-name id) value))
                      const-ids const-values)
                 (list (fn-variable-decl ids '()))
                 (map stx->javascript body))))

(define (stx->javascript/lambda ids rest-id body)
  (c:fn (if rest-id
            (append (map (lambda (id)
                           (make-Identifier #f (lexical-id-name id)))
                         ids)
                    (list (make-Identifier #f (lexical-id-name rest-id))))
            (map (lambda (id)
                   (make-Identifier #f (lexical-id-name id)))
                 ids))
        (if rest-id
            (c:= (c:var-ref (make-Identifier #f (lexical-id-name rest-id)))
                 (c:call (c:var-ref (value-name l:arguments->rest-parameter))
                         'arguments
                         (c:number (length ids))))
            (c:empty))
        (cond [(not (contains-tail-call? body))
               (c:empty)]
              [single-tail-call?
               (c:var-decl (cons 'zs '()))]
              [else
               (c:var-decl (cons 'zr (c:field (c:var-ref (value-name l:tail-object))
                                              tail-object-calls-property-name))
                           (cons 'zs '()))])
        (stx->javascript body)))

(define (stx->javascript/dispatch-lambda clauses loc)
  (c:fn '()
        (c:return
         (foldr (lambda (proc rest)
                  (match proc
                    [(s:lambda ids rest-id _)
                     (if (not rest-id)
                         (c:if-e (c:== (c:field 'arguments "length")
                                       (c:number (length ids)))
                                 (apply c:call
                                        (stx->javascript proc)
                                        (for/list ([i (in-range (length ids))])
                                          (c:field 'arguments i)))
                                 rest)
                         (c:if-e (c:>= (c:field 'arguments "length")
                                       (c:number (length ids)))
                                 (c:call (c:field (stx->javascript proc) "apply")
                                         (c:null)
                                         'arguments)
                                 rest))]))
                (c:call (c:var-ref (value-name l:error))
                        #f
                        "unexpected number of arguments")
                clauses)
         #:loc loc)))

(define (stx->javascript/blocks ids blocks)
  (match blocks
    ;; [(list (s:block frames body))
    ;;  (c:block
    ;;    (recover-continuation-code (c:var-ref (value-name l:continuation-index))
    ;;                               (c:var-ref (value-name l:continuation-state))
    ;;                               (list* (c:var-ref 'zr)
    ;;                                      (c:var-ref 'zs)
    ;;                                      (map (lambda (id)
    ;;                                             (c:var-ref (lexical-id-name id)))
    ;;                                           ids)))
    ;;    (c:try (stx->javascript tail)
    ;;           'zq
    ;;           (c:block
    ;;            (create-stack-code (c:var-ref (value-name l:continuation-state))
    ;;                               (c:var-ref (value-name l:store-continuation-exception))
    ;;                               'zq
    ;;                               (apply c:array
    ;;                                      (list* (c:var-ref 'zr) (c:var-ref 'zs)
    ;;                                             (map (lambda (id)
    ;;                                                    (c:var-ref (lexical-id-name id)))
    ;;                                                  ids))))
    ;;            (c:throw 'zq))))]
    [_
     (c:block
      (c:var-decl (cons 'zp 0))
      (recover-continuation-code (c:var-ref (value-name l:continuation-index))
                                 (c:var-ref (value-name l:continuation-state))
                                 (append (cond [(null? (blocks-with-tail-calls blocks))
                                                (list (c:var-ref 'zp))]
                                               [single-tail-call?
                                                (list (c:var-ref 'zp)
                                                      (c:var-ref 'zs))]
                                               [else
                                                (list (c:var-ref 'zp)
                                                      (c:var-ref 'zr)
                                                      (c:var-ref 'zs))])
                                         (map (lambda (id)
                                                (c:var-ref (lexical-id-name id)))
                                              ids)))
      (fn-body-block ids
                     (cond [(null? (blocks-with-tail-calls blocks)) '()]
                           [single-tail-call? (list (c:var-ref 'zs))]
                           [else                 (list (c:var-ref 'zr) (c:var-ref 'zs))] )
                     (body-switch-statement (c:var-ref 'zp) blocks)
                     blocks))]))

(define (stx->javascript/program/const/var const-global-ids const-global-values const-lexical-ids const-lexical-values global-ids lexical-ids body loc)
  (with-output-to-file "/tmp/sines-symbol-table.info"
    (lambda ()
      (printf "Global variables:~n")
      (hash-map (globals-hash)
                (lambda (k v)
                  (printf "    ~a -> ~a~n" v (module-id-name k))))
      (printf "Lexical variables:~n")
      (hash-map (lexicals-hash)
                (lambda (k v)
                  (printf "    ~a -> ~a~n" v (id-name k))))
      (printf "Property names:~n")
      (hash-map +property-hash+
                (lambda (k v)
                  (printf "    ~a -> ~a~n" v (if (module-id? k)
                                                 (module-id-name k)
                                                 k)))))
    #:exists 'replace)
  (c:expr
   (c:call
    (apply c:fn
           '()
           (fn-variable-decl lexical-ids
                             (map (lambda (id)
                                    (make-VariableInitializer
                                     #f
                                     (make-Identifier #f (variable-name id))
                                     #f))
                                  global-ids))
           (append
            (map (lambda (id value)
                   (init->javascript-statement (variable-name id) value))
                 const-global-ids const-global-values)
            (map (lambda (id value)
                   (init->javascript-statement (lexical-id-name id) value))
                 const-lexical-ids const-lexical-values)
            (list
             (c:= (c:var-ref (value-name l:current-toplevel))
                  (apply c:fn
                         '()
                         (map (lambda (stx)
                                (stx->javascript stx))
                              body)))
             (let ([continuation-trampoline (value-name l:continuation-trampoline)])
               (if (backend-wait-load? (current-backend))
                   @js{
                       window.addEventListener("DOMContentLoaded", function(ev) {
                                                                                 @continuation-trampoline() ;
                                                                                 }, false) ;
                       }
                   @js{@continuation-trampoline()}))))))))

(define (init->javascript-statement id-name value)
  (match (stx->javascript value)
    [(struct FunctionExpression [loc name args body])
     (make-FunctionDeclaration loc (make-Identifier #f id-name) args body)]
    [new-value
     (c:var-decl (make-VariableInitializer #f
                                           (make-Identifier #f id-name)
                                           new-value))]))

(define (fn-variable-decl ids additional)
  (let ([l (append (map (lambda (id)
                          (make-Identifier #f (lexical-id-name id)))
                        ids)
                   additional)])
    (if (null? l)
        (c:empty)
        (apply c:var-decl l))))

(define (contains-tail-call? node)
  (match node
    [(or (s:non-tail) (s:loop)) #f]
    [(s:tail app)
     (app-stx? app)]
    [(s:begin body)
     (ormap contains-tail-call? body)]
    [(s:if-true test then)
     (contains-tail-call? then)]
    [(s:if test then else)
     (or (contains-tail-call? then) (contains-tail-call? else))]

    [(s:begin/const/var _ const-values _ body)
     (or (ormap contains-tail-call? const-values)
         (ormap contains-tail-call? body))]
    [(s:blocks _ blocks)
     (not (null? (blocks-with-tail-calls blocks)))]
    [_ (error "javascriptify contain-tail?: not matched" node)]))

(define (blocks-with-tail-calls blocks)
  (filter (lambda (x) x)
          (for/list ([i (in-range (length blocks))]
                     [block blocks])
            (and (ormap contains-tail-call? (block-stx-body block)) i))))

;; if (!(ze instanceof c)) {
;;     if (ze instanceof b) {
;;         if ((zg!=1) && (zg!=2))
;;             ze["c"]["push"]([ zg, o3, o4, o5 ]);
;;     } else {
;;         ze = new b([ zg, o3, o4, o5 ], ze);
;;         if ((zg===1) || (zg===2))
;;             ze["c"]["push"](arguments.callee);
;;     }
;; }
;; throw ze;

(define (fn-body-block ids additional-ids body-stmt blocks)
  (c:try body-stmt
         'zq
         (c:block
          (create-stack-code (c:var-ref (value-name l:continuation-state))
                             (c:var-ref (value-name l:store-continuation-exception))
                             'zq
                             (stack-frame 'zp additional-ids ids))
          (c:throw 'zq))))

(define (body-switch-statement var blocks)
  (define (case-clause index block)
    (apply c:case
           index
           (c:= var index)
           (map stx->javascript (block-stx-body block))
           ;; (c:call 'print ;;(c:field 'console "log")
           ;;         (let ([loc (stx-loc expr)])
           ;;           (cond [(and loc (loc-source loc) (loc-line loc) (loc-column loc))
           ;;                  (string-append (path->string (loc-source loc))
           ;;                                 " "
           ;;                                 (number->string (loc-line loc))
           ;;                                 " "
           ;;                                 (number->string (loc-column loc)))]
           ;;                 [else index])))
           ))
  (apply c:switch
         var
         (for/list ([i (in-range (length blocks))]
                    [s blocks])
           (case-clause i s))))

(define (stack-frame ip additional-ids ids)
  (apply c:array ip
         (append additional-ids
                 (map (lambda (id)
                        (c:var-ref (lexical-id-name id)))
                      ids))))

(define (stack-frame2 ip ids)
  (c:array ip
           (if (null? ids)
               (c:null)
               (apply c:object
                      (map (lambda (id)
                             (cons (c:string (symbol->string (id-name id)))
                                   (c:var-ref (lexical-id-name id))))
                           ids)))))

(define (literal->javascript-expression d [loc #f])
  (cond [(boolean? d) (c:boolean d #:loc loc)]
        [(void? d)    (c:var-ref 'undefined #:loc loc)]
        [(null? d)    (c:null #:loc loc)]
        [(real? d)    (c:number (if (and (exact? d) (not (integer? d)))
                                    (exact->inexact d)
                                    d)
                                #:loc loc)]
        [(symbol? d)  (c:string (string-append (string #\uEBAC)
                                               (symbol->string d))
                                #:loc loc)]
        [(string? d)  (c:string d #:loc loc)]
        [(char? d)    (char->javascript-expression d loc)]
        [(vector? d)  (vector->javascript-expression d loc)]
        [(pair? d)    (pair->javascript-expression d loc)]
        [else         (error "invalid literal" d)]))

(define (char->javascript-expression c loc)
  (c:object (cons (c:string char->integer-property-name) (c:number (char->integer c)))
            #:loc loc))

(define (pair->javascript-expression p loc)
  (c:object (cons (c:string car-property-name) (literal->javascript-expression (car p)))
            (cons (c:string cdr-property-name) (literal->javascript-expression (cdr p)))
            #:loc loc))

(define (vector->javascript-expression v loc)
  (apply c:array
         ;; #:loc loc
         (map literal->javascript-expression (vector->list v))))

(provide/contract
 [transform (stx? . -> . Term?)])
