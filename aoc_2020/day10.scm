#!/usr/bin/env scheme-script

(import (rnrs) (util) (only (scheme) list-tail))

(define zip-shortest
  (lambda args
    (if (not (for-all pair? args)) '()
        (cons (map car args) (apply zip-shortest (map cdr args))))))

(define pairs
  (lambda (xs)
    (zip-shortest xs (cdr xs))))

(define distribution
  (lambda (ratings)
    (let ([differences (map abs (map (cut apply - <>) (pairs ratings)))])
      (* (count (cut eq? 3 <>) differences)
         (count (cut eq? 1 <>) differences)))))

(define tails
  (lambda (xs)
    (if (eq? 0 (length xs)) '()
        (cons (cdr xs) (tails (cdr xs))))))

(define combinations1
  (lambda (ratings)
    (if (<= (length ratings) 2) 1
        (let* ([current (car ratings)]
               [valid-tails (filter (lambda (xs) (and (pair? xs) (<= (- (car xs) current) 3))) (tails ratings))])
          (apply + (map combinations1 valid-tails))))))

(define list-suffix (lambda (xs n) (list-tail xs (- (length xs) n))))

(define combinations
  (lambda (ratings)
    (define cache (make-hashtable id eq? (length ratings)))
    (define helper
      (lambda (ratings)
        (or (hashtable-ref cache (car ratings) #f)
            (let* ([valid-tails (filter (lambda (xs) (and (pair? xs) (<= (- (car xs) (car ratings)) 3))) (tails ratings))]
                   [ncombinations (apply + (map helper valid-tails))])
              (hashtable-set! cache (car ratings) ncombinations)
              ncombinations))))
    (for-each (cut hashtable-set! cache <> 1) (list-suffix ratings 3))
    (helper ratings)))

(let* ([values (read-lines (open-input-file (cadr (command-line))))]
       [adapters (map string->number values)]
       [ratings (append '(0) (list-sort < adapters) (list (+ 3 (apply max adapters))))]
       [cache (make-hashtable id eq? (length ratings))])
  (write (list (distribution ratings)
               (combinations ratings))))
