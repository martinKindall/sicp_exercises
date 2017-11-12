(define nil '())

(define (entry tree)
  	(car tree)
)

(define (left-branch tree)
  	(cadr tree)
)

(define (right-branch tree)
  	(caddr tree)
)

(define (make-tree entry left right)
  	(list entry left right)
)

;---------- abstraction barrier ----------

(define (make-record identifier data)
  	(list identifier data)
)

(define (key record)
  	(car record)
)

;---------- abstraction barrier ----------

(define (list-to-tree elements)
  	(define (partial-tree elts n)
  	  	(if (= n 0)
  	  	    (cons nil elts)
  	  	    (let ((left-size (quotient (-1+ n) 2)))
  	  	    	(let ((left-result (partial-tree elts left-size)))
  	  	    		(let ((left-tree (car left-result))
  	  	    			   (non-left-elts (cdr left-result))
  	  	    			   (right-size (- n (1+ left-size)))
  	  	    		     )
  	  	    			(let ((this-entry (car non-left-elts))
  	  	    				   (right-result (partial-tree (cdr non-left-elts) right-size))
  	  	    			     )
  	  	    				(let ((right-tree (car right-result))
  	  	    					   (remaining-elts (cdr right-result))
  	  	    				     )
  	  	    					(cons (make-tree this-entry left-tree right-tree) remaining-elts)
  	  	    				)
  	  	    			)
  	  	    		)
  	  	    	)
  	  	    )
  	  	)
  	)
  	(car (partial-tree elements (length elements)))
)

(define (lookup given-key tree)
	(cond ((null? tree) false)
		  ((= given-key (key (entry tree))) (entry tree))
	      ((< given-key (key (entry tree))) (lookup given-key (left-branch tree)))
	      ((> given-key (key (entry tree))) (lookup given-key (right-branch tree)))
	)
)

(define set-of-records 
	(list-to-tree '((1 martin) (2 george) (3 samuel) (4 john) (5 tom)))
)