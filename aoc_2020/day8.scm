#!/usr/bin/env scheme-script

(import (rnrs) (util) (only (scheme) vector-copy enumerate))

(define-record-type opcode (fields instruction offset))

(define parse-op
  (lambda (line)
    (let* ([parts (string-split " " line)])
      (make-opcode (car parts) (string->number (cadr parts))))))

(define execute
  (lambda (opcodes)
    (letrec ([counts (make-hashtable id eq?)]
	     [state 0]
	     [pc 0]
	     [nop (lambda (offset) (set! pc (+ 1 pc)))]
	     [instructions (make-hashtable string-hash string=? (vector-length opcodes))]
	     [step (lambda ()
		     (let ([opcode (vector-ref opcodes pc)])
		       (hashtable-update! counts pc (cut + 1 <>) 0)
		       ((hashtable-ref instructions (opcode-instruction opcode) nop) (opcode-offset opcode)))
		     (if (or (<= 1 (hashtable-ref counts pc 0))
			     (>= pc (vector-length opcodes)))
			 (list pc state) (step)))])

      (for-each
       (cut apply (cut hashtable-set! instructions <> <>) <>)
       (list [list "acc" (lambda (offset)
			   (set! state (+ state offset))
			   (set! pc (+ 1 pc)))]
	     [list "jmp" (lambda (offset) (set! pc (+ pc offset)))]))
      (step))))

(define vector-replace
  (lambda (vec idx value)
    (let ([new (vector-copy vec)])
      (vector-set! new idx value)
      new)))

(define test
  (lambda (opcodes)
    (let* ([result (execute opcodes)]
	   [pc (car result)]
	   [state (cadr result)])
      (if (>= pc (vector-length opcodes)) state #f))))

(define candidates
  (lambda (opcodes)
    (let ([fix (lambda (idx)
		 (let* ([opcode (vector-ref opcodes idx)]
			[instruction (cond [(string=? "jmp" (opcode-instruction opcode)) "nop"]
					   [(string=? "nop" (opcode-instruction opcode)) "jmp"]
					   [else (opcode-instruction opcode)])])
		   (vector-replace opcodes idx (make-opcode instruction (opcode-offset opcode)))))])
      (car (filter id (map test (map fix (enumerate (vector->list opcodes)))))))))

(let* ([values (read-lines (open-input-file (cadr (command-line))))]
       [instructions (list->vector (map parse-op values))])
  (write (list (cadr (execute instructions))
	       (candidates instructions))))
