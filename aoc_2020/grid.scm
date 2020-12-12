(library (grid)
  (export grid grid? make-grid grid-dims grid-ref grid=? grid-copy grid-set! grid-cells)
  (import (rnrs) (only (util) cut <> zip) (only (scheme) iota vector-copy))

  (define-record-type grid (fields dims cells))

  (define unravel
    (letrec ([unravel
              (lambda (dims idx acc)
                (if (eq? idx '()) acc
                    (unravel (cdr dims) (cdr idx) (+ (car idx) (* acc (car dims))))))])
      (lambda (dims idx) (unravel dims idx 0))))

  (define grid-ref (lambda (grid idx) (vector-ref (grid-cells grid) (unravel (grid-dims grid) idx))))

  (define grid-set! (lambda (grid idx value) (vector-set! (grid-cells grid) (unravel (grid-dims grid) idx) value)))

  (define grid-copy
    (lambda (grid)
      (make-grid (grid-dims grid) (vector-copy (grid-cells grid)))))

  (define grid=?
    (lambda args
      (let* ([cellss (map grid-cells args)]
	     [dimss (map grid-dims args)]
	     [idxs (iota (apply * (car dimss)))])
	(and (for-all (lambda (dims) (for-all (cut eq? (car dims) <>) (cdr dims))) (apply zip dimss))
	     (for-all (lambda (idx) (apply eq? (map (cut vector-ref <> idx) cellss))) idxs))))))
