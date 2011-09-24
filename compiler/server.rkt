#lang scheme/base

(require (for-syntax scheme/base)
         scheme/base
         scheme/contract
         scheme/match
         scheme/runtime-path
         net/url-structs)

(require web-server/web-server
         web-server/http/response-structs
         web-server/http/request-structs
         web-server/configuration/responders)

(require (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in log: web-server/dispatchers/dispatch-log)
         (prefix-in filter: web-server/dispatchers/dispatch-filter))

(define (serve-string string)
  (lift:make (lambda (request)
               (display (request-uri request))
               (response/full 200 #"OK"
                              (current-seconds)
                              #"application/x-javacsript"
                              '()
                              (list (string->bytes/utf-8 string))))))

(define-runtime-path default-web-root
  '(lib "web-server/default-web-root"))

(define (serve-by-http url-path string #:port [port 33333])
  (serve #:port port
         #:dispatch (sequencer:make (log:make #:log-path "dispatch.log")
                                    (filter:make
                                     (regexp url-path)
                                     (serve-string string))
                                    (lift:make
                                     (gen-file-not-found-responder
                                      (build-path default-web-root
                                                  "conf"
                                                  "not-found.html")))))
  (display (format "Server started. Enjoy!~%"))
  (do-not-return))

(provide/contract
 (serve-by-http (->* [string? string?] [#:port integer?] any)))
