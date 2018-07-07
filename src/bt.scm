(define-module (bt)
  #:use-module (system foreign)
  #:export (<brownian-tree>
	    bt-init
	    bt-quit))

(define bt-lib (dynamic-link "./libbrownian_tree.so"))
(define bt-init
  (pointer->procedure '* 
		      (dynamic-func "bt_init" bt-lib)
		      (list uint64 uint64 uint32)))

(define bt-new-particle-at
  (pointer->procedure int32 ; C Boolean, 1 true : 0 false
		      (dynamic-func "bt_new_particle_at" bt-lib)
		      (list uint64 uint64))) ; Co-ordinates from which to start

(define bt-new-seed-at
  (pointer->procedure void
		      (dynamic-func "bt_new_seed_at" bt-lib)
		      (list uint64 uint64))) ; Co-ordinates for new seed
(define bt-quit
  (pointer->procedure void
		      (dynamic-func "bt_destroy" bt-lib)
		      (list)))

(define bt-struct-type (list uint64 ; x, width
			     uint64 ; y, height
			     '* ; buffer, memory
			     uint32 ; Random seed
			     '* ; State of random number generator.
			     uint64 ; Number of particles.
			     uint64 ; out of bound steps
			     uint64 ; Total number of steps
			     uint64)) ; successful traversals

(define node-struct-type (list int32 ; Type of node,
					; Enumerated Empty=0, Seed=1, Particle = 2
			       int32 ; Other Attributes
			       uint64 ; Depth of the node from any numbered seed
			       uint64 ; Steps walked to create the node
			       uint64 ; point from which the walk was started X-
			       uint64 ; ^^ Y-
			       '*)) ; Lowest depth connected node 
(define (bt-size b)
  (let ((width (list-ref (parse-c-struct b bt-struct-type) 0))
	(height (list-ref (parse-c-struct b bt-struct-type) 1)))
    (cons width height)))

(define (bt-seed b)
  (list-ref (parse-c-struct b bt-struct-type) 3))

(define (bt-particle-size b)
  (list-ref (parse-c-struct b bt-struct-type) 5))

(define (wrap-brownian-tree bt-pointer)
  )

(define (unwrap-brownian-tree bt-pointer)
  )


(define-wrapped-pointer-type <brownian-tree>
  brownian-tree?
  wrap-brownian-tree unwrap-brownian-tree
  (lambda (b p)
    (format p "#<brownian tree of size: ~a ~a ~a>"
	    (bt-size b)
	    (bt-seed b)
	    (bt-particle-size b))))
