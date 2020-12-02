#!/usr/bin/env scheme-script

(import (rnrs))

(letrec ([file (car (cdr (command-line)))]
		 [mult-to (lambda (x y z) (eq? (* x y) z))]
		 [take-until
		  (lambda (c f acc)
			(let ([v (f)])
			  (if (c v) acc (take-until c f (cons v acc)))))]
		 [read-lines (lambda (in) (take-until eof-object? (lambda () (get-line in)) '()))]
		 [cartesian
		  (lambda (xs ys acc)
			(cond
			 [(eq? (length xs) 0) acc]
			 [else (cartesian (cdr xs) ys (append (map (lambda (y) (cons (car xs) y)) ys) acc))]))]
		 [matching (lambda (xs) (filter (lambda (xs) (eq? 2020 (fold-left + 0 xs))) xs))])
  (let ([values (map string->number (read-lines (open-input-file file)))]
		[product (lambda (xs) (fold-left * 1 xs))])
	(display (list
			  (car (map product (matching (cartesian values (map (lambda (x) (list x)) values) '()))))
			  (car (map product (matching (cartesian values (cartesian values (map (lambda (x) (list x)) values) '()) '()))))))))
