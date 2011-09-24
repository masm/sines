#lang racket

(require racket/cmdline
         racket/runtime-path)

(define backend
  (command-line #:args (name)
                name))

(define-runtime-path backend-path "backend.rkt")

(let ([form (with-handlers ([exn:fail:filesystem?
                             (lambda (_) #f)])
              (with-input-from-file backend-path read))]
      [new-form `(module backend racket/base
                   (define backend ,backend)
                   (provide backend))])
  (unless (equal? form new-form)
    (delete-file backend-path)
    (make-file-or-directory-link (string-append "backend/" backend ".rkt")
                                 backend-path)))

