(library (util)
  (export cut <> take-until read-lines zip count string-find string-split list-split interleave string-join writeln id cartesian)
  (import (rnrs))

  (define id (lambda (id) id))

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
    (letrec ([string-find
              (lambda (query str pos)
                (cond [(eq? (+ -1 pos (string-length query)) (string-length str)) #f]
                      [(eq? 0 (string-length query)) #f]
                      [(string=? query (substring str pos (+ pos (string-length query)))) pos]
                      [else (string-find query str (+ 1 pos))]))])
      (case-lambda [(query str pos) (string-find query str pos)]
                   [(query str) (string-find query str 0)])))

  (define string-split
    (lambda (sep str)
      (letrec ([string-split
                (lambda (sep pos acc)
                  (let ([next (string-find sep str pos)])
                    (cond [(eq? pos (string-length str)) acc]
                          [next (cons (substring str pos next)
                                      (string-split sep (+ (string-length sep) next) acc))]
                          [else (cons (substring str pos (string-length str)) acc)])))])
        (string-split (if (char? sep) (string sep) sep) 0 '()))))

  (define interleave
    (lambda (sep xss)
      (letrec ([interleave
                (lambda (xss acc)
                  (if (eq? '() xss) acc
                      (interleave (cdr xss) (cons (car xss) (cons sep acc)))))])
        (cons (car xss) (reverse (interleave (cdr xss) '()))))))

  (define string-join (lambda (sep strings) (apply string-append (interleave sep strings))))

  (define writeln
    (lambda args
      (apply write args)
      (display #\newline)))

  (define cartesian
    (lambda args
      (fold-right
       (lambda (xs ys)
	 (apply append (map (lambda (x) (map (lambda (y) (cons x y)) ys)) xs)))
      '(()) args))))
