(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(define (square-tree tree)
  	(cond ((null? tree) ())
  	      ((not (pair? tree)) (square tree))
  	      (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
  	)
)

(define (map proc items)
  	(if (null? items)
  	    '()
  	    (cons (proc (car items)) (map proc (cdr items)))
  	)
)

(define (square-tree-v2 tree)
  	(map 
  		(lambda (subtree) 
  			(cond ((pair? subtree) (square-tree-v2 subtree))
  			      (else (square subtree))
  			)
  		)
  		tree
  	)
)