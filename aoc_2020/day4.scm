#!/usr/bin/env scheme-script

(import (rnrs) (util))

(define parse-passport
  (lambda (lines)
    (map (cut string-split #\: <>) (list-sort string>? (string-split #\space (apply string-append (interleave " " lines)))))))

(define is-valid
  (lambda (passport)
    (let ([keys (map car passport)])
      (if (exists (cut string=? "cid" <>) keys)
          (eq? 8 (length keys))
          (eq? 7 (length keys))))))

(define between
  (lambda (min max v) (and (>= v min) (<= v max))))

(define string-suffix
  (lambda (str n)
    (substring str (- (string-length str) n) (string-length str))))

(define string-tosuffix (lambda (str n) (substring str 0 (- (string-length str) n))))
(define string-fromprefix (lambda (str n) (substring str n (string-length str))))

(define startswith
  (lambda (str prefix)
    (string=? prefix (substring str 0 (string-length prefix)))))

(define endswith
  (lambda (str suffix)
    (string=? suffix (string-suffix str (string-length suffix)))))

(define is-hex
  (lambda (str)
    (cond [(string? str) (for-all is-hex (string->list str))]
          [(char? str) (or (between (char->integer #\0) (char->integer #\9) (char->integer str))
                           (between (char->integer #\a) (char->integer #\f) (char->integer str)))])))

(define is-decimal
  (lambda (str)
    (cond [(string? str) (for-all is-decimal (string->list str))]
          [(char? str) (between (char->integer #\0) (char->integer #\9) (char->integer str))])))

(define rules
  (let ([rules (make-hashtable string-hash string=?)])
    (for-each (cut apply hashtable-set! rules <>)
              (list (list "byr" (lambda (byr) (between 1920 2002 (string->number byr))))
                    (list "iyr" (lambda (iyr) (between 2010 2020 (string->number iyr))))
                    (list "eyr" (lambda (eyr) (between 2020 2030 (string->number eyr))))
                    (list "hgt" (lambda (hgt) (cond [(endswith hgt "cm") (between 150 193 (string->number (string-tosuffix hgt 2)))]
                                                    [(endswith hgt "in") (between 59 76 (string->number (string-tosuffix hgt 2)))]
                                                    [else #f])))
                    (list "hcl" (lambda (hcl) (and (eq? 7 (string-length hcl)) (startswith hcl "#")
                                                   (is-hex (string-fromprefix hcl 1)))))
                    (list "ecl" (lambda (ecl) (exists (cut string=? ecl <>) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))
                    (list "pid" (lambda (pid) (and (is-decimal pid) (eq? 9 (string-length pid)))))
                    (list "cid" (lambda _ #t))))
    rules))

(define is-valid-entry (lambda (entry) ((hashtable-ref rules (car entry) (lambda _ #f)) (cadr entry))))

(define is-valid-2 (lambda (passport) (and (is-valid passport) (for-all is-valid-entry passport))))

(let* ([values (read-lines (open-input-file (car (cdr (command-line)))))]
       [passports (map parse-passport (list-split "" values))])
  (write (list (count is-valid passports)
               (count is-valid-2 passports))))
