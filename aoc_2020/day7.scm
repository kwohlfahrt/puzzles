#!/usr/bin/env scheme-script

(import (rnrs) (util) (only (scheme) list-head list-tail hashtable-cells))

(define list-init (lambda (xs n) (list-head xs (- (length xs) n))))

(define-record-type content (fields count colour))

(define string->content
  (lambda (str)
    (let ([args (string-split " " str)]
          [parse-count (lambda (count) (if (string=? "no" count) 0 (string->number count)))])
      (make-content (parse-count (car args)) (string-join " " (cdr args))))))

(define content=?
  (lambda (x y)
    (and (apply eq? (map content-count (list x y)))
         (apply string=? (map content-colour (list x y))))))

(define parse-rule
  (lambda (rule)
    (let* ([halves (string-split " contain " rule)]
           [outer (string-join " " (list-init (string-split #\space (car halves)) 1))]
           [trim-rule (lambda (raw) (string-join " " (list-init (string-split #\space raw) 1)))]
           [contents (map string->content (map trim-rule (string-split ", " (cadr halves))))])
      (list outer contents))))

(define parse-rules
  (lambda (rules)
    (let ([ht (make-hashtable string-hash string=?)])
      (for-each (cut apply (cut hashtable-set! ht <> <>) <>) (map parse-rule rules))
      ht)))

(define can-contain
  (lambda (rules color target)
    (let ([contents (map content-colour (hashtable-ref rules color '()))])
      (or (exists (cut string=? target <>) contents)
          (exists (cut can-contain rules <> target) contents)))))

(define bag-size
  (lambda (rules colour)
    (let ([contents (hashtable-ref rules colour '())])
      (+ (apply + (map content-count contents))
         (apply + (map (lambda (content)
                         (* (content-count content)
                            (bag-size rules (content-colour content)))) contents))))))

(let* ([values (read-lines (open-input-file (cadr (command-line))))]
       [rules (parse-rules values)])
  (write (list (count (cut can-contain rules <> "shiny gold") (vector->list (hashtable-keys rules)))
               (bag-size rules "shiny gold"))))
