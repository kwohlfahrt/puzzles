#!/usr/bin/env scheme-script

(import (rnrs) (util))

(define parse-range
  (lambda (str) (map string->number (string-split #\- str))))

(define range-contains
  (lambda (range val) (and (<= val (cadr range)) (>= val (car range)))))

(define parse-rule
  (lambda (str)
    (let ([sections (string-split #\space str)])
      (list (parse-range (list-ref sections 0))
            (string-ref (list-ref sections 1) 0)
            (list-ref sections 2)))))

(define check-rule
  (lambda (range char password)
    (range-contains range (count (cut eq? char <>) (string->list password)))))

(define check-rule-2
  (lambda (range char password)
    (eq? 1 (length (filter (cut eq? char <>) (map (lambda (pos) (string-ref password (- pos 1))) range))))))

(let ([values (read-lines (open-input-file (car (cdr (command-line)))))])
  (display (list
            (count (cut apply check-rule <>) (map parse-rule values))
            (count (cut apply check-rule-2 <>) (map parse-rule values)))))
