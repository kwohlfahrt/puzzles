#!/usr/bin/env scheme-script

(import (rnrs) (util))

(define group-answers
  (lambda (answers)
    (let ([ht (make-hashtable char->integer char=?)])
      (string-for-each (cut hashtable-update! ht <> (cut + 1 <>) 0) (apply string-append answers))
      ht)))

(define count-answers (lambda (answers) (hashtable-size (group-answers answers))))

(define hashtable-values
  (lambda (ht)
    (call-with-values (lambda () (hashtable-entries ht)) (lambda (_ values) values))))

(define count-all
  (lambda (answers)
    (let ([ht (group-answers answers)])
      (count (cut eq? (length answers) <>) (vector->list (hashtable-values ht))))))

(let* ([values (read-lines (open-input-file (cadr (command-line))))])
  (write (list (apply + (map count-answers (list-split "" values)))
               (apply + (map count-all (list-split "" values))))))
