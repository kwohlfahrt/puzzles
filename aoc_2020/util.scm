(library (util)
  (export read-lines take-until)
  (import (rnrs))

  (define take-until
    (lambda (c f acc)
      (let ([v (f)])
        (if (c v) acc (take-until c f (cons v acc))))))
  (define read-lines
    (lambda (in) (take-until eof-object? (lambda () (get-line in)) '()))))
