(library (util)
  (export cut <> take-until read-lines zip count)
  (import (rnrs))

  (define-syntax cut-internal
    (syntax-rules (<>)
      [(_ (slot-name ...) (position ...) <> . args) (cut-internal (slot-name ... x) (position ... x) . args)]
      [(_ (slot-name ...) (position ...) nse . args) (cut-internal (slot-name ...) (position ... nse) . args)]
      [(_ (slot-name ...) (proc args ...)) (lambda (slot-name ...) (proc args ...))]))


  (define-syntax cut
    (syntax-rules ()
      [(_ . args) (cut-internal () () . args)]))

  (define <>) ; For REPL compatibility

  (define take-until
    (letrec ([take-until
              (lambda (c f acc)
                (let ([v (f)])
                  (if (c v) acc (take-until c f (cons v acc)))))])
      (lambda (c f) (reverse (take-until c f '())))))

  (define read-lines
    (lambda (in) (take-until eof-object? (lambda () (get-line in)))))

  (define zip (lambda args (apply map (cons list args))))

  (define count (lambda (fn xs) (length (filter fn xs)))))

