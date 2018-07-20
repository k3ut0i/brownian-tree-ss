#!/usr/bin/guile \ 
-e main -s
!#
(use-modules (ice-9 regex)
	     (ice-9 getopt-long)
	     (ice-9 rdelim)
	     (srfi srfi-43))

(define dump-exe "dump_test.bin")

(define option-spec
  '((help (single-char #\h) (value #f))
    (verbose (single-char #\v) (value #f))
    (num-of-tests (single-char #\n) (value #t))))

;;;TODO: Complete option handling
(define (main args)
  (let ((options (getopt-long args option-spec)))
    (display options)))

(define (run-dump-test width height rseed npart outfile)
  (system* dump-exe width height rseed npart outfile))

(define node-regex
  (make-regexp (string-concatenate
		(list "\\[([0-9]+),([0-9]+)\\]\t" ; Current Node 1, 2
		      "([0-9]+)\t" ; Node Type(EMPTY=1, SEED=2, PART=3) 3
		      "([0-9]+)\t"             ; 4
		      "([0-9]+)\t"             ; 5
		      "([0-9]+)\t"             ; 6
		      "\\[([0-9]+),([0-9]+)\\]")))); From Node 7, 8

(define size-pattern "([0-9].*)\t([0-9].*)")

(define (print-buffer port)
  #f)

(define (get-size str)
  (let ((size-match (string-match size-pattern str)))
    (cons (string->number (match:substring size-match 1))
	  (string->number (match:substring size-match 2)))))

(define (get-node-attributes str)
  (let ((node-match (regexp-exec node-regex str)))
    (list->vector (map (lambda (i)
			 (string->number (match:substring node-match i)))
		       (list 1 2 3 4 5 6 7 8)))))

(define (node->pixel attr)
  (if (= (vector-ref attr 2) 0) 0 1))

; dump is in node order, so ideally we can pipe input and ouput without any storage.
(define (dump->pbm in out)
  (let ((size (get-size (read-line in))))
    (let loop ((line (read-line in)))
      (if (eof-object? line)
	  (begin (format out "~A\t" (node->pixel (get-node-attributes line)))
		 (loop (read-line in)))
	  (format #t "Completed Conversion~%")))))

(define (dump->pbm-file in-file out-file)
  (let ((in-port #f)
	(out-port #f))
    (dynamic-wind
      (lambda ()
	(set! in-port (open-input-file in-file))
	(set! out-port (open-output-file out-file)))
      (lambda ()
	(dump->pbm in-port out-port))
      (lambda ()
	(close-port in-port)
	(close-port out-port)))))
