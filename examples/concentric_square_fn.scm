(define (rgn-generator size)
  "Return points of a concentric square half the SIZE."
  (let* ((i 0)
	 (side 0)
	 (s (quotient size 2))
	 (s1 (quotient s 2))
	 (s3 (+ s1 s)))
    (lambda ()
      (let ((ans (case side
		   ((0) (cons (+ s1 i) s1)) ;Botton
		   ((1) (cons s3 (+ s1 i))) ;Right
		   ((2) (cons (+ s1 i) s3)) ;Top
		   ((3) (cons s1 (+ s1 i)))
		   (else #f))))
	(if (= s (+ i 1))
	    (begin (set! i 0)
		   (set! side (+ side 1)))
	    (set! i (+ i 1)))
	ans))))
