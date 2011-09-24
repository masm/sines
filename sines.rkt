#lang scheme/base

(require
 scheme/base
 scheme/cmdline
 scheme/list
 scheme/match
 scheme/path
 scheme/pretty
 scheme/runtime-path
 (prefix-in p: "externals/javascript/print.rkt")
 "compiler/code-store.rkt"
 "compiler/compiler.rkt"
 "compiler/primitives.rkt"
 "compiler/syntax.rkt"
 "compiler/deserialize.rkt"
 "compiler/server.rkt"
 )

(define url-path (make-parameter #f))

(define backend  (make-parameter "spidermonkey"))

(define compress? (make-parameter #f))

(define dump-phases? (make-parameter #f))

(define dump-phase (make-parameter #f))

(define timings? (make-parameter #f))

(define-values (file-to-compile)
  (command-line
   #:once-any [("-s" "--http") p "Start http server"
               (url-path p)]
   #:once-each [("-b" "--backend") b "Backend"
                (backend b)]
               [("-c" "--compress") "Compress output"
                (compress? #t)]
               [("-d" "--dump") "Dump code of compiler stages"
                (dump-phases? #t)]
               [("-p" "--dump-pass") name "Dump code of compiler stage"
                (dump-phase (transformation-name->index name))]
               [("-t" "--timings") "Display pass timings"
                (timings? #t)]

   #:args (path)
   (values (normalize-path path))))

(define-runtime-module-path code-store-path "compiler/code-store.rkt")
(define-runtime-module-path compiler-path "compiler/compiler.rkt")
(define-runtime-module-path syntax-path "compiler/syntax.rkt")

(define-namespace-anchor namespace-anchor)
(define-values (namespace code)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-attach-module (namespace-anchor->empty-namespace namespace-anchor)
                             syntax-path)
    (dynamic-require file-to-compile #f)
    (let ([l ((dynamic-require code-store-path 'code-list))])
      (values (current-namespace) (reverse l)))))

(define (do-compile)
  (display
   (cond [(compress?)
          (parameterize ([p:collapse-lines? #t]
                         [p:collapse-simple-substatements? #t]
                         [p:current-indentation-width 0])
            (cond [(dump-phase) => (lambda (i) (modules->javascript-string/dump code #:pass i #:display-timings (timings?)))]
                  [(dump-phases?) (modules->javascript-string/dump code #:display-timings (timings?))]
                  [else (modules->javascript-string code #:display-timings (timings?))]))]
          [else
           (cond [(dump-phase) => (lambda (i) (modules->javascript-string/dump code #:pass i #:display-timings (timings?)))]
                 [(dump-phases?) (modules->javascript-string/dump code #:display-timings (timings?))]
                 [else (modules->javascript-string code #:display-timings (timings?))])])))

(define (do-serve)
  (serve-by-http (url-path)
                 (modules->javascript-string code)))

(cond [(url-path) (do-serve)]
      [else       (do-compile)])
