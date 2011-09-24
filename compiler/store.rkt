#lang scheme

(require syntax/id-table)

(define primitive-transformers (make-free-id-table))

(define (primitive-transformer id)
  (dict-ref primitive-transformers id))

(define (set-primitive-transformer! id proc)
  (dict-set! primitive-transformers id proc))

(define-struct prototype-fields (constructor-name
                                 maker-name
                                 predicate-name
                                 slot-names
                                 getter-names
                                 param-names
                                 property-names))

(provide primitive-transformer
         set-primitive-transformer!
         (struct-out prototype-fields))
