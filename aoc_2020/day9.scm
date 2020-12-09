#!/usr/bin/env scheme-script

(import (rnrs) (util) (only (scheme) iota))

(define-record-type slice (fields base start stop))

(define slice-length (lambda (slice) (- (slice-stop slice) (slice-start slice))))

(define slice-ref
  (lambda (slice idx)
    (let ([base (slice-base slice)]
	  [idx (if (>= idx 0) (+ idx (slice-start slice))
		   (- (slice-stop slice) (abs idx)))])
      ((if (slice? base) slice-ref vector-ref) base idx))))

(define slice->vector
  (lambda (slice)
    (let* ([stop (slice-stop slice)]
	   [start (slice-start slice)]
	   [dst (make-vector (- stop start))])
      (for-each (lambda (idx) (vector-set! dst idx (slice-ref slice idx))) (iota (- stop start)))
      dst)))

(define slice->list (lambda (slice) (vector->list (slice->vector slice))))

(define windows
  (lambda (v n)
    (map (lambda (start) (make-slice v start (+ start n)))
	 (iota (+ 1 (- (vector-length v) n))))))

(define is-valid
  (lambda (group)
    (let* ([candidate (slice-ref group -1)]
	   [preamble (slice->list (make-slice group 0 (- (slice-length group) 1)))]
	   [sums (map (cut apply + <>) (cartesian preamble preamble))])
      (exists (cut eq? candidate <>) sums))))

(define find-odd
  (lambda (values size)
    (let ([groups (windows values (+ 1 size))])
      (slice-ref (car (filter (lambda (group) (not (is-valid group))) groups)) -1))))

(define find-weakness
  (lambda (values size)
    (define key (find-odd values size))
    (define start 0)
    (define end 2)
    (define search-range
      (lambda ()
	(let* ([range (slice->list (make-slice values start end))]
	       [sum (apply + range)])
	  (cond [(eq? sum key) range]
		[(< sum key) (begin
			       (set! end (+ 1 end))
			       (search-range))]
		[(> sum key) (begin
			       (set! start (+ 1 start))
			       (search-range))]))))
    (let ([range (search-range)])
      (+ (apply min range) (apply max range)))))

(let* ([values (read-lines (open-input-file (cadr (command-line))))]
       [codes (list->vector (map string->number values))])
  (write (list (find-odd codes 25)
	       (find-weakness codes 25))))
