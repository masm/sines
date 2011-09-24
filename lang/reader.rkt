(module reader syntax/module-reader
  #:language `(file ,(path->string (resolved-module-path-name sines-lang-path)))

  (require scheme/runtime-path)
  (define-runtime-module-path sines-lang-path "../sines-lang.rkt"))
