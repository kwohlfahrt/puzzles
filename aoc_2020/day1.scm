#!/usr/bin/env scheme-script

(import (rnrs) (util))

(define cartesian
  (case-lambda
   [(xs ys) (cartesian xs ys '())]
   [(xs ys acc)
    (cond [(eq? (length xs) 0) acc]
          [else (cartesian (cdr xs) ys (append (map (cut cons (car xs) <>) ys) acc))])]))

(define sum (cut fold-left + 0 <>))
(define product (cut fold-left * 1 <>))

(let ([values (map string->number (read-lines (open-input-file (car (cdr (command-line))))))]
      [rule (lambda (xs) (eq? 2020 (sum xs)))])
  (display (list
            (car (map product (filter rule (cartesian values (map list values)))))
            (car (map product (filter rule (cartesian values (cartesian values (map list values)))))))))
