#!/usr/bin/env scheme-script

(import (rnrs) (util) (only (scheme) iota) (grid))

(define parse-map
  (lambda (values)
    (let ([h (length values)]
	  [w (string-length (car values))])
      (make-grid (list h w) (list->vector (string->list (apply string-append values)))))))

(define is-empty? (cut eq? #\L <>))
(define is-occupied? (cut eq? #\# <>))

(define rule1
  (lambda (seat neighbours)
    (cond [(and (is-empty? seat) (not (exists (cut is-occupied? <>) neighbours))) #\#]
	  [(and (is-occupied? seat) (>= (count is-occupied? neighbours) 4)) #\L]
	  [else seat])))

(define rule2
  (lambda (seat neighbours)
    (cond [(and (is-empty? seat) (not (exists (cut is-occupied? <>) neighbours))) #\#]
	  [(and (is-occupied? seat) (>= (count is-occupied? neighbours) 5)) #\L]
	  [else seat])))

(define in-bounds
  (lambda (dims idxs)
    (for-all
     (lambda (args)
       (let ([dim (car args)]
	     [idx (cadr args)])
	 (and (>= idx 0) (< idx dim))))
     (zip dims idxs))))

(define neighbours
  (lambda (grid idxs)
    (map (cut grid-ref grid <>)
	 (filter
	  (cut in-bounds (grid-dims grid) <>)
	  (neighbour-idxs idxs)))))

(define neighbour-idxs
  (lambda (idxs)
    (define adjacent
      (lambda (idx) (list idx (+ idx 1) (- idx 1))))
    (cdr (apply cartesian (map adjacent idxs)))))

(define step
  (lambda (grid visibility rule)
    (let ([idxs (apply cartesian (map iota (grid-dims grid)))]
	  [new-grid (grid-copy grid)])
      (for-each
       (lambda (idx)
	 (grid-set! new-grid idx (rule (grid-ref grid idx) (visibility grid idx))))
       idxs)
      new-grid)))

(define play
  (lambda (grid visibility rule)
    (let ([new-grid (step grid visibility rule)])
      (if (grid=? grid new-grid) grid
	  (play new-grid visibility rule)))))

(define count-occupied
  (lambda (grid) (count (cut eq? #\# <>) (vector->list (grid-cells grid)))))

(define visible
  (lambda (grid idxs)
    (define directions (neighbour-idxs '(0 0)))
    (define next-seat
      (lambda (idx direction)
	(let ([step (map (cut apply + <>) (zip idx direction))])
	  (cond [(not (in-bounds (grid-dims grid) step)) #\.]
		[(eq? #\. (grid-ref grid step)) (next-seat step direction)]
		[else (grid-ref grid step)]))))
    (map (cut next-seat idxs <>) directions)))

(let* ([values (read-lines (open-input-file (cadr (command-line))))]
       [seats (parse-map values)])
  (write (list (count-occupied (play seats neighbours rule1))
	       (count-occupied (play seats visible rule2)))))
