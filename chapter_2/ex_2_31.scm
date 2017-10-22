(define (map proc items)
  	(if (null? items)
  	    '()
  	    (cons (proc (car items)) (map proc (cdr items)))
  	)
)

(define (tree-map proc tree)
  	(map 
  		(lambda (subtree) 
  			(cond ((pair? subtree) (tree-map proc subtree))
  			      (else (proc subtree))
  			)
  		)
  		tree
  	)
)

(define (square-tree tree)
  	(tree-map square tree)
)
