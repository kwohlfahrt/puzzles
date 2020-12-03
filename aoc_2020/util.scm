(library (util)
  (export cut <> take-until read-lines)
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
    (lambda (c f acc)
      (let ([v (f)])
        (if (c v) acc (take-until c f (cons v acc))))))

  (define read-lines
    (lambda (in) (take-until eof-object? (lambda () (get-line in)) '()))))
