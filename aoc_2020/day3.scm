#!/usr/bin/env scheme-script
(import (rnrs) (only (chezscheme) sleep make-time) (util) (grid))

(define is-tree (cut eq? #\# <>))

(define roll (lambda (dims idx) (map mod idx dims)))

(define parse-grid
  (lambda (lines)
    (make-grid
     (list (length lines) (string-length (car lines)))
     (list->vector (apply append (map (lambda (line) (map is-tree (string->list line))) lines))))))

(define intersections
  (letrec ([intersections
            (lambda (grid slope pos acc)
              (if (>= (car pos) (car (grid-dims grid))) acc
                  (intersections grid slope (apply map (cons + (list slope pos))) (cons (grid-ref grid (roll (grid-dims grid) pos)) acc))))])
    (lambda (grid slope) (reverse (intersections grid slope '(0 0) '())))))

(define count-trees (lambda (grid slope) (count (cut eq? #t <>) (intersections grid slope))))

(let ([grid (parse-grid (read-lines (open-input-file (car (cdr (command-line))))))]
      [slopes '((1 1) (1 3) (1 5) (1 7) (2 1))])
  (display (list (count-trees grid '(1 3)) (apply * (map (cut count-trees grid <>) slopes)))))
