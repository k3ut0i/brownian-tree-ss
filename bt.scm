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
(define-wrapped-pointer-type brownian-tree
  brownian-tree?
  wrap-brownian-tree unwrap-brownian-tree
  (lambda (b p)
    (format p "#<brownian tree of size: ~a ~a ~a>"
	    (bt-size b)
	    (bt-seed b)
	    (bt-particle-size b))))
(define brownian-tree-struct (list uint64 ; x, width
				   uint64 ; y, height
				   '* ; buffer, memory
				   uint32 ; Random seed
				   '* ; State of random number generator.
				   uint64 ; Number of particles.
				   uint64 ; out of bound steps
				   uint64 ; Total number of steps
				   uint64 ; successful traversals
				   ))
(define node-struct (list int32 ; Type of node,
					; Enumerated Empty=0, Seed=1, Particle = 2
			  int32 ; Other Attributes
			  uint64 ; Depth of the node from any numbered seed
			  uint64 ; Steps walked to create the node
			  uint64 ; point from which the walk was started X-
			  uint64 ; ^^ Y-
			  '* ; Lowest depth connected node 
			  ))
(define (bt-size b)
  (let ((width (list-ref (parse-c-struct b brownian-tree-struct) 0))
	(height (list-ref (parse-c-struct b brownian-tree-struct) 1)))
    (cons width height)))

(define (bt-seed b)
  (list-ref (parse-c-struct b brownian-tree-struct) 3))

(define (bt-particle-size b)
  (list-ref (parse-c-struct b brownian-tree-struct) 5))
