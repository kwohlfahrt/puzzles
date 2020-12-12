#!/usr/bin/env scheme-script

(import (rnrs) (util) (only (scheme) iota) (grid))

(define-record-type state (fields facing pos))

(define direction
  (lambda (facing)
    (case (mod facing 360)
      [(0) '(1 0)]
      [(90) '(0 1)]
      [(180) '(-1 0)]
      [(270) '(0 -1)])))

(define-record-type instruction (fields action value))

(define parse-instruction
  (lambda (line)
    (make-instruction (string-ref line 0) (string->number (substring line 1 (string-length line))))))

(define move
  (lambda (pos direction n)
    (let ([offset (map (cut * n <>) direction)])
      (map + pos offset))))

(define step1
  (lambda (state instruction)
    (let ([value (instruction-value instruction)]
          [facing (state-facing state)]
          [pos (state-pos state)])
      (case (instruction-action instruction)
        [(#\N) (make-state facing (move pos '(1 0) value))]
        [(#\S) (make-state facing (move pos '(-1 0) value))]
        [(#\E) (make-state facing (move pos '(0 1) value))]
        [(#\W) (make-state facing (move pos '(0 -1) value))]
        [(#\L) (make-state (- facing value) pos)]
        [(#\R) (make-state (+ facing value) pos)]
        [(#\F) (make-state facing (move pos (direction facing) value))]))))

(define manhattan (lambda (pos) (apply + (map abs pos))))

(define-record-type state-2 (fields waypoint pos))

(define rotate
  (lambda (pos degrees)
    (let ([y (car pos)]
          [x (cadr pos)])
      (case (mod degrees 360)
        [(0) (list y x)]
        [(90) (list (- x) y)]
        [(180) (list (- y) (- x))]
        [(270) (list x (- y))]))))

(define step2
  (lambda (state instruction)
    (let ([value (instruction-value instruction)]
          [waypoint (state-2-waypoint state)]
          [pos (state-2-pos state)])
      (case (instruction-action instruction)
        [(#\N) (make-state-2 (move waypoint '(1 0) value) pos)]
        [(#\S) (make-state-2 (move waypoint '(-1 0) value) pos)]
        [(#\E) (make-state-2 (move waypoint '(0 1) value) pos)]
        [(#\W) (make-state-2 (move waypoint '(0 -1) value) pos)]
        [(#\L) (make-state-2 (rotate waypoint (- value)) pos)]
        [(#\R) (make-state-2 (rotate waypoint value) pos)]
        [(#\F) (make-state-2 waypoint (move pos waypoint value))]))))

(define evaluate
  (lambda (state step instructions)
    (if (not (pair? instructions)) state
        (evaluate (step state (car instructions)) step (cdr instructions)))))


(let* ([values (read-lines (open-input-file (cadr (command-line))))]
       [instructions (map parse-instruction values)])
  (write (list (manhattan (state-pos (evaluate (make-state 90 '(0 0)) step1 instructions)))
               (manhattan (state-2-pos (evaluate (make-state-2 '(1 10) '(0 0)) step2 instructions))))))
