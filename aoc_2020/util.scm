(library (util)
  (export cut <> take-until read-lines zip count string-split unravel list-split)
  (import (rnrs))

  (define list-split
    (lambda (sep xs)
      (letrec ([list-split
                (lambda (xs acc)
                  (cond [(eq? xs '()) acc]
                        [(eq? sep (car xs)) (list-split (cdr xs) (cons '() acc))]
                        [else (list-split (cdr xs) (cons (cons (car xs) (car acc)) (cdr acc)))]))])
        (reverse (list-split xs '(()))))))

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

  (define count (lambda (fn xs) (length (filter fn xs))))

  (define string-find
    (case-lambda
     [(c str pos)
      (cond [(eq? pos (string-length str)) #f]
            [(eq? c (string-ref str pos)) pos]
            [else (string-find c str (+ 1 pos))])]
     [(c str) (string-find c str 0)]))

  (define string-split
    (lambda (sep str)
      (letrec ([string-split
                (lambda (pos acc)
                  (let ([next (string-find sep str pos)])
                    (cond [(eq? pos (string-length str)) acc]
                          [next (cons (substring str pos next)
                                      (string-split (+ 1 next) acc))]
                          [else (cons (substring str pos (string-length str)) acc)])))])
        (string-split 0 '()))))

  (define unravel
    (letrec ([unravel
              (lambda (dims idx acc)
                (if (eq? idx '()) acc
                    (unravel (cdr dims) (cdr idx) (+ (car idx) (* acc (car dims))))))])
      (lambda (dims idx) (unravel dims idx 0)))))
