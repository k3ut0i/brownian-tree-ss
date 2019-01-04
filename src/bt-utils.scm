(define-module (bt utils)
  #:use-module (bt)
  #:export (with-brownian-tree))

;;; TODO: Rather than carrying the tree state into every function
;;; try writing a macro whose lexical closure handles trivial
;;; things. should i use dynamic wind?

(define-syntax with-brownian-tree
  (syntax-rules ()
    ((_ (tree x-size y-size random-seed) body)
     (let ((tree (new-brownian-tree (cons x-size y-size) random-seed)))
       ,@body
       (%bt-destroy (raw-pointer tree))))))


(define (new-seeds-with tree proc)
  "return the number of seeds set."
  (let lp ((pos (proc))
	   (num 0))
    (if pos
	(begin (%new-seed-at tree (car pos) (cdr pos))
	       (lp (proc) (+ num 1)))
	num)))

(define (new-particles-with tree proc)
  "Introduce particles generated from proc"
  (let lp ((pos (proc))
	   (num 0))
    (if pos
	(if (new-particle-from pos)
	    (lp (proc) (+ num 1))
	    (lp (proc) num))
	num)))
