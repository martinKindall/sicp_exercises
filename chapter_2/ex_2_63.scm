(define nil '())

(define (accumulate op initial sequence)
  	(cond ((null? sequence) initial)
  	      (else (op (car sequence) (accumulate op initial (cdr sequence))))
  	)
)

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

(define (element-of-set? x tree)
  	(cond ((null? tree) false)
  	      ((= x (entry tree)) true)
  	      ((< x (entry tree)) (element-of-set? x (left-branch tree)))
  	      ((> x (entry tree)) (element-of-set? x (right-branch tree)))
  	)
)

(define (adjoin-set x tree)
    (cond ((null? tree) (make-tree x nil nil))
          ((= x (entry tree)) tree)
          ((< x (entry tree)) (make-tree (entry tree) (adjoin-set x (left-branch tree)) (right-branch tree)))
          ((> x (entry tree)) (make-tree (entry tree) (left-branch tree) (adjoin-set x (right-branch tree))))
    )
)

(define (adjoin-set-list aList tree)
  	(accumulate adjoin-set tree (reverse aList))

  	; procudes same output, but using accumulate is more elegant
  	;(if (null? aList)
  	;    tree
  	;    (adjoin-set-list (cdr aList) (adjoin-set (car aList) tree))
  	;)
)

(define (treeToList1 tree)
  	(if (null? tree)
  	    nil
  	    (append (treeToList1 (left-branch tree)) (cons (entry tree) (treeToList1 (right-branch tree))))
  	)
)

(define (treeToList2 tree)
  	(define (copy-to-list tree result)
  	  	(if (null? tree)
  	  	    result
  	  	    (copy-to-list (left-branch tree) (cons (entry tree) (copy-to-list (right-branch tree) result)))
  	  	)
  	)
  	(copy-to-list tree nil)
)

(define tree1 (make-tree 7 
				(make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil))
				(make-tree 9 nil (make-tree 11 nil nil))
			  )
)

(define tree2 (make-tree 3 
				(make-tree 1 nil nil)
				(make-tree 7 (make-tree 5 nil nil) (make-tree 9 nil (make-tree 11 nil nil)))
			  )
)

(define tree3 (make-tree 5 
				(make-tree 3 (make-tree 1 nil nil) nil)
				(make-tree 9 (make-tree 7 nil nil) (make-tree 11 nil nil))
			  )
)

(define tree4 (adjoin-set-list '(2 3 4 5 6 7) (make-tree 1 nil nil)))

(define listTree (list tree1 tree2 tree3 tree4))

(for-each (lambda (x)(newline)(display (treeToList1 x))(display (treeToList2 x))) listTree)