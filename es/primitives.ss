#lang s-exp "../base-lang.rkt"

(require
 "bridge.ss")

(define-identifier-syntax es-+nan+       (es-global "NaN"))
(define-identifier-syntax es-+infinity+  (es-global "Infinity"))
(define-identifier-syntax es-+undefined+ (es-global "undefined"))

(define-identifier-syntax es-+object-proto+          (es-global "Object"))
(define-identifier-syntax es-+function-proto+        (es-global "Function"))
(define-identifier-syntax es-+array-proto+           (es-global "Array"))
(define-identifier-syntax es-+string-proto+          (es-global "String"))
(define-identifier-syntax es-+boolean-proto+         (es-global "Boolean"))
(define-identifier-syntax es-+number-proto+          (es-global "Number"))
(define-identifier-syntax es-+date-proto+            (es-global "Date"))
(define-identifier-syntax es-+error-proto+           (es-global "Error"))
(define-identifier-syntax es-+eval-error-proto+      (es-global "EvalError"))
(define-identifier-syntax es-+range-error-proto+     (es-global "RangeError"))
(define-identifier-syntax es-+reference-error-proto+ (es-global "ReferenceError"))
(define-identifier-syntax es-+syntax-error-proto+    (es-global "SyntaxError"))
(define-identifier-syntax es-+type-error-proto+      (es-global "TypeError"))
(define-identifier-syntax es-+uri-error-proto+       (es-global "URIError"))
(define-identifier-syntax es-*self*                  (c-es-this))

;;; Basic access

(define (es-object? obj)
  (eq? (es-typeof obj) "object"))

;;; Native ECMAScript Objects

(define-primitive-invoker (es-string-eval string) (es-global "eval"))

(define-primitive-invoker (es-number-nan? number) (es-global "isNaN"))

(define-primitive-invoker (es-number-finite-number? number) (es-global "isFinite"))

;;; Object objects

;;  (define-primitive-invoker (es-object value) (es-global "Object"))

(define-getter (es-length object) "length")
(define-invoker (es-object-to-locate-string object) "toLocaleString")
(define-invoker (es-object-this-value object) "valueOf")
(define-invoker (es-object-has-own-property? object string) "hasOwnProperty")
(define-invoker (es-object-is-prototype-of? object other-object) "isPrototypeOf")
(define-invoker (es-object-property-is-enumerable? object property-name) "propertyIsEnumerable")
(define-getter (es-constructor object) "constructor")

(define (es-object-has-property? obj string)
  (es-in? string obj))

(provide es-object
         es-length
         es-object-to-locate-string
         es-object-this-value
         es-object-has-own-property?
         es-object-is-prototype-of?
         es-object-property-is-enumerable?
         es-object-has-property?
         es-constructor)

;;; Function objects

  (define-getter (es-function->arity function) "length")

  (define-getter+setter (es-function-prototype function) "prototype")

  (define-invoker (es-function-apply function this-object arg-array)
    "apply")

  ;; arguments also has caller and length

  (define-getter (es-callee) "callee" (es-arguments))

;;; Array objects

  ;; constructor, toString, toLocaleString, concat

  (define-invoker (es-array-index-of array search-element from-index)
    "indexOf") ; Gecko (not in standard)

  (define-invoker (es-array-last-index-of array search-element from-index)
    "lastIndexOf") ; Gecko (not in standard)


 ;; TODO: check out Array and String generics in JavaScript 1.6; and 1.7 and 1.8

;;; String objects

 ;; constructor, toString, valueOf

;;; Boolean objects

  (define-primitive-invoker (es-boolean value) (es-global "Boolean"))

;;; Number objects

  (define-primitive-invoker (es-number value) (es-global "Number"))

  (define-getter (es-number-max-value) "MAX_VALUE" (es-global "Number"))

  (define-getter (es-number-min-value) "MIN_VALUE" (es-global "Number"))

  (define-getter (es-number-nan) "NaN" (es-global "Number"))

  (define-getter (es-number-negative-infinity) "NEGATIVE_INFINITY" (es-global "Number"))

  (define-getter (es-number-positive-infinity) "POSITIVE_INFINITY" (es-global "Number"))

;;; Math object

  (define-getter (es-math-e) "E" (es-global "Math"))

  (define-getter (es-math-ln10) "LN10" (es-global "Math"))

  (define-getter (es-math-ln2) "LN2" (es-global "Math"))

  (define-getter (es-math-log2e) "LOG2E" (es-global "Math"))

  (define-getter (es-math-log10e) "LOG10E" (es-global "Math"))

  (define-getter (es-math-pi) "PI" (es-global "Math"))

  (define-getter (es-math-sqrt1/2) "SQRT1_2" (es-global "Math"))

  (define-getter (es-math-sqrt2) "SQRT2" (es-global "Math"))

;;; Date objects

  (define-invoker (es-string->millis string)
    "parse" (es-global "Date"))

  (define (es-string->date date-string)
    (es-new (es-global "Date") date-string))

  (define-invoker (es-date->date-string date)
    "toDateString")

  (define-invoker (es-date->time-string date)
    "toTimeString")

  (define-invoker (es-date->locale-date-string date)
    "toLocaleDateString")

  (define-invoker (es-date->locale-time-string date)
    "toLocaleTimeString")

  (define-invoker (es-date->time-set! date time)
    "setTime")

  (define-invoker (es-date->millis-set! date millis)
    "setMilliseconds")

  (define-invoker (es-date->utc-millis-set! date millis)
    "setUTCMilliseconds")

  (define-invoker (es-date->seconds-set! date seconds)
    "setSeconds")

  (define-invoker (es-date->utc-seconds-set! date seconds)
    "setUTCSeconds")

  (define-invoker (es-date->minutes-set! date minutes)
    "setMinutes")

  (define-invoker (es-date->utc-minutes-set! date minutes)
    "setUTCMinutes")

  (define-invoker (es-date->hours-set! date hours)
    "setHours")

  (define-invoker (es-date->utc-hours-set! date hours)
    "setUTCHours")

  (define-invoker (es-date->date-set! date day)
    "setDate")

  (define-invoker (es-date->utc-date-set! date day)
    "setUTCDate")

  (define-invoker (es-date->month-set! date month)
    "setMonth")

  (define-invoker (es-date->utc-month-set! date month)
    "setUTCMonth")

  (define-invoker (es-date->full-year-set! date full-year)
    "setFull")

  (define-invoker (es-date->utc-full-year-set! date full-year)
    "setUTCFull")

  (define-invoker (es-date->utc-string date)
    "toUTCString")

;;; Error objects

  (define-getter (es-error-name error) "name")

  (define-getter (es-error-message error) "message")

  (define-invoker (es-function-call function this-object . args)
    "call")

  (define-invoker (es-array-copy-and-add array . items)
    "concat")

  (define-invoker (es-array-concat array . other-array)
    "concat")

  (define-invoker (es-string-from-char-codes . chars)
    "fromCharCode" (es-global "String"))

  (define-invoker (es-string-concat string . other-strings)
    "concat")

  (define-invoker (es-math-max . values)
    "max" (es-global "Math"))

  (define-invoker (es-math-min . values)
    "min" (es-global "Math"))

(provide es-+nan+
         es-+infinity+
         es-+undefined+
         es-+object-proto+
         es-+function-proto+
         es-+array-proto+
         es-+string-proto+
         es-+boolean-proto+
         es-+number-proto+
         es-+date-proto+
         es-+error-proto+
         es-+eval-error-proto+
         es-+range-error-proto+
         es-+reference-error-proto+
         es-+syntax-error-proto+
         es-+type-error-proto+
         es-+uri-error-proto+
         es-*self*

         es-object?
         es-string-eval
         es-number-nan?
         es-number-finite-number?
         es-function->arity
         es-function-prototype
         es-function-apply
         es-callee

         es-array-index-of
         es-array-last-index-of

         es-boolean

         es-number
         es-number-max-value
         es-number-min-value
         es-number-nan
         es-number-negative-infinity
         es-number-positive-infinity

         es-math-e
         es-math-ln10
         es-math-ln2
         es-math-log2e
         es-math-log10e
         es-math-pi
         es-math-sqrt1/2
         es-math-sqrt2

         es-string->millis
         es-string->date
         es-date->date-string
         es-date->time-string
         es-date->locale-date-string
         es-date->locale-time-string
         es-date->time-set!
         es-date->millis-set!
         es-date->utc-millis-set!
         es-date->seconds-set!
         es-date->utc-seconds-set!
         es-date->minutes-set!
         es-date->utc-minutes-set!
         es-date->hours-set!
         es-date->utc-hours-set!
         es-date->date-set!
         es-date->utc-date-set!
         es-date->month-set!
         es-date->utc-month-set!
         es-date->full-year-set!
         es-date->utc-full-year-set!
         es-date->utc-string

         es-error-name
         es-error-message

         es-function-call
         es-array-copy-and-add
         es-array-concat
         es-string-from-char-codes
         es-string-concat
         es-math-max
         es-math-min
         )
