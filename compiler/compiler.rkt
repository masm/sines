#lang racket/base

(require
 racket/base
 racket/list
 racket/port
 racket/pretty
 "../externals/javascript/print.rkt"
 (prefix-in optimize: "../externals/javascript/optimizer.rkt")
 (prefix-in pprint:   (planet dherman/pprint))
 "backend.rkt"
 "deserialize.rkt"
 "primitives.rkt"
 "syntax.rkt"
 (prefix-in s: "syntax2.rkt")
 (prefix-in case-lambda-conversion:    "passes/case-lambda-conversion.rkt")
 (prefix-in unify-ids:                 "passes/unify-ids.rkt")
 (prefix-in early-primitive-expansion: "passes/early-primitive-expansion.rkt")
 (prefix-in letrec-fix:                "passes/letrec-fix.rkt")
 (prefix-in boxify:                    "passes/boxify.rkt")
 (prefix-in contraction:               "passes/contraction.rkt")
 (prefix-in auto-inlining:             "passes/auto-inlining.rkt")
 (prefix-in inline-expansion:          "passes/inline-expansion.rkt")
 (prefix-in loopify:                   "passes/loopify.rkt")
 (prefix-in wcm-conversion:            "passes/wcm-conversion.rkt")
 (prefix-in anf:                       "passes/anf.rkt")
 (prefix-in let-conversion:            "passes/let-conversion.rkt")
 (prefix-in literals:                  "passes/literals.rkt")
 (prefix-in define-conversion:         "passes/define-conversion.rkt")
 (prefix-in optimization2:             "passes/optimization2.rkt")
 (prefix-in tail-form:                 "passes/tail-form.rkt")
 (prefix-in pull-procs:                "passes/pull-procs.rkt")
 (prefix-in label-bodies:              "passes/label-bodies.rkt")
 (prefix-in merge-blocks:              "passes/merge-blocks.rkt")
 (prefix-in optimization3:             "passes/optimization3.rkt")
 (prefix-in statementize:              "passes/statementize.rkt")
 (prefix-in javascriptify:             "passes/javascriptify.rkt"))

(define transformations
  (list (cons 'case-lambda-conversion    case-lambda-conversion:transform)
        (cons 'unify-ids                 unify-ids:transform)
        (cons 'early-primitive-expansion early-primitive-expansion:transform)
        (cons 'letrec-fix                letrec-fix:transform)
        (cons 'boxify                    boxify:transform)

        (cons 'contraction1              contraction:transform)
        (cons 'auto-inlining             auto-inlining:transform)
        ;; (cons 'inline-expansion          inline-expansion:transform)
        (cons 'contraction2              contraction:transform)
        ;; (cons 'loopify                   loopify:transform)
        ;; (cons 'contraction3              contraction:transform)
        ;; (cons 'auto-inlining             auto-inlining:transform)
        ;; ;; (cons 'inline-expansion          inline-expansion:transform)
        ;; (cons 'contraction4              contraction:transform)

        (cons 'wcm-conversion            wcm-conversion:transform)
        (cons 'anf                       anf:transform)
        (cons 'let-conversion            let-conversion:transform)
        (cons 'literals                  literals:transform)
        (cons 'define-conversion         define-conversion:transform)
        (cons 'optimization2             optimization2:transform)
        (cons 'tail-form                 tail-form:transform)
        (cons 'pull-procs                pull-procs:transform)
        (cons 'label-bodies              label-bodies:transform)
        (cons 'merge-blocks              merge-blocks:transform)
        (cons 'optimization3             optimization3:transform)
        (cons 'statementize              statementize:transform)))

(define transformation-name->index
  (let ([h (make-hash)])
    (for-each (lambda (transformation i)
                (hash-set! h (symbol->string (car transformation)) i))
              transformations
              (build-list (length transformations) values))
    (lambda (name) (hash-ref h name (lambda () (error "unknown pass" name))))))

(define (program->javascript-string program timings?)
  (define (doit)
    (let ([code (foldl (lambda (transformation code)
                         (maybe-timings timings? (car transformation) (lambda () ((cdr transformation) code))))
                       program
                       transformations)])
      (maybe-timings timings? 'javascriptify (lambda () (javascriptify:transform code)))))
  (maybe-timings timings? "Total" doit))

(define (maybe-timings timings? name thunk)
  (if timings?
      (let-values ([(ret cpu real gc) (time-apply thunk '())])
        (fprintf (current-error-port) "~A cpu time: ~A real time: ~A gc time: ~A~%" (if (string? name) name (string-upcase (symbol->string name))) cpu real gc)
        (apply values ret))
      (thunk)))

(define (program->javascript-string/dump program pass timings?)
  (define (doit)
    (unless pass
      (call-with-output-file "/tmp/sines-00.scm"
        #:exists 'replace
        (lambda (out)
          (parameterize ([pretty-print-columns 159])
            (pretty-print (code->sexp program) out)))))
    (let ([code (foldl (lambda (transformation i code)
                         (let ([new-code (maybe-timings timings? (car transformation) (lambda () ((cdr transformation) code)))])
                           (when (and (> i 0)
                                      (or (not pass) (= pass i)))
                             (call-with-output-file (format "/tmp/sines-~A-~A.scm" (if (< i 10)
                                                                                       (string-append "0" (number->string i))
                                                                                       i)
                                                            (car transformation))
                               #:exists 'replace
                               (lambda (out)
                                 (parameterize ([pretty-print-columns 159])
                                   (pretty-print (code->sexp new-code) out)))))
                           new-code))
                       program
                       transformations
                       (build-list (length transformations) values))])
      (maybe-timings timings? 'javascriptify (lambda () (javascriptify:transform code)))))
  (maybe-timings timings? "Total" doit))

(define (modules->program-stx modules)
  (s:program (append-map module-stx-body modules)))

(define (modules->javascript-string modules #:display-timings [timings? #f])
  (let ([program (modules->program-stx modules)])
    (let ([code (stx->javascript-string (program->javascript-string program timings?))])
      (cond [(backend-header (current-backend)) => (lambda (s) (string-append s code))]
            [else code]))))

(define (modules->javascript-string/dump modules #:display-timings [timings? #f] #:pass [pass #f])
  (let ([program (modules->program-stx modules)])
    (let ([code (stx->javascript-string (program->javascript-string/dump program pass timings?))])
      (cond [(backend-header (current-backend)) => (lambda (s) (string-append s code))]
            [else code]))))

(define (stx->javascript-string js)
  (with-output-to-string
    (lambda ()
      (pprint:pretty-print (format-source-element (optimize:optimize js))))))

(provide transformation-name->index
         modules->javascript-string
         modules->javascript-string/dump)
