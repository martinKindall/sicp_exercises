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

(define (union-set set1 set2)
  	(define (iter set1 set2 res)
  	  	(cond ((null? set1) (append res set2))
  	  	      ((null? set2) (append res set1))
  	  	      ((= (car set1) (car set2)) (iter (cdr set1) (cdr set2) (append res (list (car set1)))))
  	  	      ((< (car set1) (car set2)) (iter (cdr set1) set2 (append res (list (car set1)))))
  	  	      ((> (car set1) (car set2)) (iter set1 (cdr set2) (append res (list (car set2)))))
  	  	)
  	)
  	(iter set1 set2 nil)
)

(define (intersection-set set1 set2)
  	(if (or (null? set1) (null? set2))
  	    '()
  	    (let (
  	    	  (x1 (car set1)) 
  	    	  (x2 (car set2))
  	         )
  	    	(cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
  	    	      ((< x1 x2) (intersection-set (cdr set1) set2))
  	    	      ((< x2 x1) (intersection-set set1 (cdr set2)))
  	    	)
  	    )
  	)
)

(define (tree-to-list tree)
  	(define (copy-to-list set result)
  	  	(if (null? set)
  	  	    result
  	  	    (copy-to-list (left-branch set) (cons (entry set) (copy-to-list (right-branch set) result)))
  	  	)
  	)
  	(copy-to-list tree nil)
)

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

(define (union-set-tree tree1 tree2)
	(let ((list1 (tree-to-list tree1))
		   (list2 (tree-to-list tree2))
	     )
	  	(list-to-tree (union-set list1 list2))
	)  	
)

(define tree1 (list-to-tree '(1 2 3 4 5 6)))
(define tree2 (list-to-tree '(5 6 7 8 9 10)))

; (union-set-tree tree1 tree2) should output the same tree as tree3

(define tree3 (list-to-tree '(1 2 3 4 5 6 7 8 9 10)))


(define (intersection-set-tree tree1 tree2)
	(let ((list1 (tree-to-list tree1))
		   (list2 (tree-to-list tree2))
	     )
	  	(list-to-tree (intersection-set list1 list2))
	)  	
)


(define tree4 (list-to-tree '(5 6)))