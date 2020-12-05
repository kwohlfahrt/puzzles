#!/usr/bin/env scheme-script

(import (rnrs) (util))

(define midpoint (lambda (range) (/ (+ (car range) (cadr range)) 2)))

(define walk-tree
  (let ([step (lambda (direction range)
                (if direction
                    (list (car range) (midpoint range))
                    (list (midpoint range) (cadr range))))])
    (lambda (directions range)
      (if (eq? directions '()) range
          (walk-tree (cdr directions) (step (car directions) range))))))

(define find-seat
  (lambda (pass)
    (let ([row-direction (map (cut eq? #\F <>) (string->list (substring pass 0 7)))]
          [col-direction (map (cut eq? #\L <>) (string->list (substring pass 7 10)))])
      (list (car (walk-tree row-direction '(0 128)))
            (car (walk-tree col-direction '(0 8)))))))

(define seat-id (cut unravel '(128 8) <>))

(define pairs
  (lambda (xs)
    (if (< (length xs) 2) '()
        (cons (list (car xs) (cadr xs)) (pairs (cdr xs))))))

(let* ([values (read-lines (open-input-file (car (cdr (command-line)))))]
       [seat-ids (map seat-id (map find-seat values))])
  (write (list (apply max seat-ids)
               (midpoint (car (filter (lambda (pair) (not (eq? 1 (apply - pair)))) (pairs (list-sort > seat-ids))))))))
