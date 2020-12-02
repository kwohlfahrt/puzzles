#!/usr/bin/env scheme-script

(import (rnrs) (util))

(define string-find
  (case-lambda
   [(c str pos)
    (cond [(eq? pos (string-length str)) #f]
          [(eq? c (string-ref str pos)) pos]
          [else (string-find c str (+ 1 pos))])]
   [(c str) (string-find c str 0)]))

(define string-split
  (letrec ([string-split
            (lambda (sep str pos acc)
              (let ([next (string-find sep str pos)])
                (cond [(eq? pos (string-length str)) acc]
                      [next (cons (substring str pos next)
                                  (string-split sep str (+ 1 next) acc))]
                      [else (cons (substring str pos (string-length str)) acc)])))])
    (lambda (sep str) (string-split sep str 0 '()))))

(define count (lambda (fn xs) (length (filter fn xs))))

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
    (range-contains range (count (lambda (c) (eq? char c)) (string->list password)))))

(define check-rule-2
  (lambda (range char password)
    (eq? 1 (length (filter (lambda (c) (eq? c char)) (map (lambda (pos) (string-ref password (- pos 1))) range))))))

(let ([values (read-lines (open-input-file (car (cdr (command-line)))))])
  (display (list
            (count (lambda (rule) (apply check-rule rule)) (map parse-rule values))
            (count (lambda (rule) (apply check-rule-2 rule)) (map parse-rule values)))))
