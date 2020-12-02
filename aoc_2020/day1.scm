#!/usr/bin/env scheme-script

(import (rnrs) (util))

(define cartesian
  (case-lambda
   [(xs ys) (cartesian xs ys '())]
   [(xs ys acc)
    (cond [(eq? (length xs) 0) acc]
          [else (cartesian (cdr xs) ys (append (map (lambda (y) (cons (car xs) y)) ys) acc))])]))

(define sum (lambda (xs) (fold-left + 0 xs)))
(define product (lambda (xs) (fold-left * 1 xs)))

(let ([values (map string->number (read-lines (open-input-file (car (cdr (command-line))))))]
      [rule (lambda (xs) (eq? 2020 (sum xs)))])
  (display (list
            (car (map product (filter rule (cartesian values (map (lambda (x) (list x)) values)))))
            (car (map product (filter rule (cartesian values (cartesian values (map (lambda (x) (list x)) values)))))))))
