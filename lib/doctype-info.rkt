#lang racket/base

(struct doctype-info
  (dict proc-name)
  #:property prop:rename-transformer (struct-field-index proc-name))

(provide
 doctype-info
 doctype-info-dict)
