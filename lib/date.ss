#lang s-exp "../base-lang.rkt"

(require
 "../es/bridge.ss"
 "regexp.ss")

;;; Making dates

(define (current-date)
  (es-new (es-global "Date")))

(define (make-date y mo d h mi s milli)
  (es-new (es-global "Date") y (sub1 mo) d h mi s milli))

(define (make-date/utc y mo d h mi s milli)
  (milliseconds->date (es-call-property (es-global "Date") "UTC" y (sub1 mo) d h mi s milli)))

;; (define-invoker (es-utc-date->milliseconds y mo d h mi s millis)
;;   "UTC" (es-global "Date"))

(define (milliseconds->date millis)
  (es-new (es-global "Date") millis))

(define (date? obj)
  (es-instance? obj (es-global "Date")))

(define (date=? a b)
  (= (date->milliseconds a) (date->milliseconds b)))

(define (date<? a b)
  (< (date->milliseconds a) (date->milliseconds b)))

(define (date>? a b)
  (> (date->milliseconds a) (date->milliseconds b)))

(define (date<=? a b)
  (<= (date->milliseconds a) (date->milliseconds b)))

(define (date>=? a b)
  (>= (date->milliseconds a) (date->milliseconds b)))

(provide current-date
         make-date
         make-date/utc
         milliseconds->date
         date?
         date=?
         date<?
         date>?
         date<=?
         date>=?)

;;; Converting

(define-invoker (date->milliseconds date)
  "getTime")

(define (date->iso-string date)
  (string-append (date->iso-date-string date) " " (date->iso-time-string date)))

(define (date->iso-date-string date)
  (string-append (date-year date) "-"
                 (pad-xx-number (date-month date)) "-"
                 (pad-xx-number (date-day date))))

(define (date->iso-time-string date)
  (string-append (pad-xx-number (date-hours date)) ":"
                 (pad-xx-number (date-minutes date)) ":"
                 (pad-xx-number (date-seconds date))))

(define (date->iso-string/utc date)
  (string-append (date->iso-date-string/utc date) " " (date->iso-time-string/utc date)))

(define (date->iso-date-string/utc date)
  (string-append (date-year/utc date) "-"
                 (pad-xx-number (date-month/utc date)) "-"
                 (pad-xx-number (date-day/utc date))))

(define (date->iso-time-string/utc date)
  (string-append (pad-xx-number (date-hours/utc date)) ":"
                 (pad-xx-number (date-minutes/utc date)) ":"
                 (pad-xx-number (date-seconds/utc date))))

(define (pad-xx-number x)
  (if (< x 10)
      (string-append "0" x)
      x))

(define date-parsing-regexp
  (regexp "^(\\d{4})(?:-(\\d{1,2})(?:-(\\d{1,2})(?: (\\d{1,2})(?::(\\d{1,2})(?::(\\d{1,2}))?)?)?)?)?$"))

(define (iso-string->date str)
  (cond [(regexp-match date-parsing-regexp str)
         => (lambda (l)
              (let ([y  (cadr l)]
                    [mo (caddr l)]
                    [d  (cadddr l)]
                    [h  (car (cddddr l))]
                    [mi (cadr (cddddr l))]
                    [s  (caddr (cddddr l))])
                (make-date (string->number y)
                           (if (es-undefined? mo) 1 (string->number mo))
                           (if (es-undefined? d)  1 (string->number d))
                           (if (es-undefined? h)  0 (string->number h))
                           (if (es-undefined? mi) 0 (string->number mi))
                           (if (es-undefined? s)  0 (string->number s))
                           0)))]
        [else #f]))

(define (iso-string->date/utc str)
  (cond [(regexp-match date-parsing-regexp str)
         => (lambda (l)
              (let ([y  (cadr l)]
                    [mo (caddr l)]
                    [d  (cadddr l)]
                    [h  (car (cddddr l))]
                    [mi (cadr (cddddr l))]
                    [s  (caddr (cddddr l))])
                (make-date/utc (string->number y)
                               (if (es-undefined? mo) 1 (string->number mo))
                               (if (es-undefined? d)  1 (string->number d))
                               (if (es-undefined? h)  0 (string->number h))
                               (if (es-undefined? mi) 0 (string->number mi))
                               (if (es-undefined? s)  0 (string->number s))
                               0)))]
        [else #f]))

(provide date->milliseconds
         date->iso-string
         date->iso-date-string
         date->iso-time-string
         date->iso-string/utc
         date->iso-date-string/utc
         date->iso-time-string/utc
         iso-string->date
         iso-string->date/utc)

;;; Accessing compontents

(define-invoker (date-year date) "getFullYear")
(define-invoker (date-year/utc date) "getUTCFullYear")
(define-invoker (_date-month date) "getMonth")

(define (date-month date)
  (add1 (_date-month date)))

(define-invoker (_date-month/utc date) "getUTCMonth")

(define (date-month/utc date)
  (add1 (_date-month/utc date)))

(define-invoker (date-day date)     "getDate")
(define-invoker (date-day/utc date) "getUTCDate")
(define-invoker (%day-of-week date) "getDay")

(define (day-of-week date)
  (let ([n (%day-of-week date)])
    (if (zero? n) 6 (sub1 n))))

(define-invoker (%day-of-week/utc date) "getUTCDay")

(define (day-of-week/utc date)
  (let ([n (%day-of-week/utc date)])
    (if (zero? n) 6 (sub1 n))))

(define-invoker (date-hours date)            "getHours")
(define-invoker (date-hours/utc date)        "getUTCHours")
(define-invoker (date-minutes date)          "getMinutes")
(define-invoker (date-minutes/utc date)      "getUTCMinutes")
(define-invoker (date-seconds date)          "getSeconds")
(define-invoker (date-seconds/utc date)      "getUTCSeconds")
(define-invoker (date-milliseconds date)     "getMilliseconds")
(define-invoker (date-milliseconds/utc date) "getUTCMilliseconds")
(define-invoker (date-timezone-offset date)  "getTimezoneOffset")

(provide date-year
         date-year/utc
         date-month
         date-month/utc
         date-day
         date-day/utc
         day-of-week
         day-of-week/utc
         date-hours
         date-hours/utc
         date-minutes
         date-minutes/utc
         date-seconds
         date-seconds/utc
         date-milliseconds
         date-milliseconds/utc
         date-timezone-offset)

;;; Utilities

(define (beginning-of-day date)
  (make-date (date-year date)
             (date-month date)
             (date-day date)
             0 0 0 0))

(define (beginning-of-week date)
  (make-date (date-year date)
             (date-month date)
             (- (date-day date) (day-of-week date))
             0 0 0 0))

(define (beginning-of-month date)
  (make-date (date-year date)
             (date-month date)
             1 0 0 0 0))

(define (beginning-of-year date)
  (make-date (date-year date)
             0 1 0 0 0 0))

(define (next-second date [c 1])
  (milliseconds->date (+ (date->milliseconds date) (* c 1000))))

(define (previous-second date [c 1])
  (next-second date (- c)))

(define (next-minute date [c 1])
  (milliseconds->date (+ (date->milliseconds date) (* c 1000 60))))

(define (previous-minute date [c 1])
  (next-minute date (- c)))

(define (next-hour date [c 1])
  (milliseconds->date (+ (date->milliseconds date) (* c 1000 60 60))))

(define (previous-hour date [c 1])
  (next-hour date (- c)))

;; (define (next-day date [c 1])
;;   (milliseconds->date (+ (date->milliseconds date) (* c 1000 60 60 24))))

(define (next-day date [c 1])
  (make-date (date-year date)
             (date-month date)
             (+ (date-day date) c)
             (date-hours date)
             (date-minutes date)
             (date-seconds date)
             (date-milliseconds date)))

(define (previous-day date [c 1])
  (next-day date (- c)))

;; (define (next-week date [c 1])
;;   (milliseconds->date (+ (date->milliseconds date) (* c 1000 60 60 24 7))))

(define (next-week date [c 1])
  (make-date (date-year date)
             (date-month date)
             (+ (date-day date) (* 7 c))
             (date-hours date)
             (date-minutes date)
             (date-seconds date)
             (date-milliseconds date)))

(define (previous-week date [c 1])
  (next-week date (- c)))

(define (next-month date [c 1])
  (let ([new-date (make-date (date-year date)
                             (+ (date-month date) c)
                             (date-day date)
                             (date-hours date)
                             (date-minutes date)
                             (date-seconds date)
                             (date-milliseconds date))])
    (if (= (date-month new-date)
           (let ([m (mod (+ (date-month date) c) 12)])
             (if (zero? m) 12 m)))
        new-date
        (make-date (date-year new-date)
                   (date-month new-date)
                   0
                   (date-hours new-date)
                   (date-minutes new-date)
                   (date-seconds new-date)
                   (date-milliseconds new-date)))))

(define (previous-month date [c 1])
  (next-month date (- c)))

(provide beginning-of-day
         beginning-of-week
         beginning-of-month
         beginning-of-year
         next-second
         previous-second
         next-minute
         previous-minute
         next-hour
         previous-hour
         next-day
         previous-day
         next-week
         previous-week
         next-month
         previous-month)

;;; Alternative names

(define day-after  next-day)
(define day-before previous-day)
(define week-after  next-week)
(define week-before previous-week)
(define month-after  next-month)
(define month-before previous-month)

(provide day-after
         day-before
         week-after
         week-before
         month-after
         month-before)