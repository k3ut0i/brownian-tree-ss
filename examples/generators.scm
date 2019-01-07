(define (square-generator size)
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
		   ((3) (cons s1 (+ s1 i))) ;Left
		   (else #f))))
	(if (= s (+ i 1))
	    (begin (set! i 0)
		   (set! side (+ side 1)))
	    (set! i (+ i 1)))
	ans))))

(define pi 3.141592654)

(define (circle-generator size)
  "Return points of a concentric circle half the SIZE."
  (let* ((r (quotient (- size 1) 4))
	 (n (ceiling (* 2 (* pi r))))
	 (theta (/ 1 r)))
    (lambda ()
      (let ((ans (if (< n 0)
		     #f
		     (let* ((xa (+ (* r (cos (* n theta))) r))
			    (ya (+ (* r (sin (* n theta))) r))
			    (x (round (inexact->exact (+ r xa))))
			    (y (round (inexact->exact (+ r ya)))))
		       (cons x y)))))
	(set! n (- n 1))
	ans))))
