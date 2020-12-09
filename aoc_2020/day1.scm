#!/usr/bin/env scheme-script

(import (rnrs) (util))

(define sum (cut fold-left + 0 <>))
(define product (cut fold-left * 1 <>))

(define rule (lambda (xs) (eq? 2020 (sum xs))))

(let ([values (map string->number (read-lines (open-input-file (car (cdr (command-line))))))])
  (display (list
	    (car (map product (filter rule (cartesian values values))))
	    (car (map product (filter rule (cartesian values values values)))))))
