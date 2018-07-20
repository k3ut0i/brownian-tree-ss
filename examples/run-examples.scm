#!/usr/bin/guile -s
!#
(define scripted-seeds "./scripted_seeds.bin")

(define (run-program path args)
  (if (access? path X_OK)
      (apply system* path args)
      ;; Throw something. need a non local exit from here
      (throw 'program-access-error path)))

(define (run-scripted-seeds size rseed npart outfile scriptfile)
  (let ((args (list size rseed npart outfile scriptfile))
	(data->string (lambda (d)
			(if (number? d)
			    (number->string d)
			    d))))
    (format #t "Running ~A ~A ~A~%" size rseed npart)
    (run-program scripted-seeds (map data->string args))))

(define sizes (list 100 200 300 400 500))
(define rseeds (list 1 2 3 4 5 6 7 8 9 10))
(define nparticles (list 0.01 0.02 0.03)) ; size*size*ratio

(define (create-sequence script-file)
  (let* ((all-combinations (cart-prod3 sizes rseeds nparticles))
	 (all-args (map (lambda (c)
			  (let ((s (car c))
				(r (cadr c))
				(part-ratio (caddr c)))
			    (list s r (floor (* part-ratio s s)))))
			all-combinations))
	 (output-name (lambda (args)
			(format #f "example_~S_~S_~S.pbm"
				(car args)
				(cadr args)
				(caddr args)))))
    (map (lambda (args)
	   (apply run-scripted-seeds (append args
					     (list (output-name args)
						   script-file))))
	 all-args)))

;;; Cartesian product of two sets, a combination function
(define (cart-prod s1 s2 f)
  (let loop ((ans (list))
	     (t1 s1))
    (if (null? t1)
	ans
	(loop (append ans (map (lambda (e)
				 (f (car t1) e))
			       s2))
	      (cdr t1)))))

;;; Regular cartesian product of three sets
(define (cart-prod3 s1 s2 s3)
  (let ((t1 (cart-prod s1 s2 list)))
    (cart-prod t1 s3 (lambda (a b)
		      (append a (list b))))))

(define (pbm->jpg path))
(define (ppm->jpg path))
