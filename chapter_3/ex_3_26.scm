; the following example works for multi dimensional tables

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
  	(cons identifier data)
)

(define (key record)
  	(car record)
)

(define (record-value record)
  	(cdr record)
)

;---------- abstraction barrier ----------

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

(define (lookup-tree given-key tree)
	(cond ((null? tree) false)
		  ((= given-key (key (entry tree))) (entry tree))
	      ((< given-key (key (entry tree))) (lookup-tree given-key (left-branch tree)))
	      ((> given-key (key (entry tree))) (lookup-tree given-key (right-branch tree)))
	)
)

(define (adjoin-record-tree! record tree)
	;(debug tree)
	(cond ((null? tree) false)
          ((= (key record) (key (entry tree))) tree)
          ((< (key record) (key (entry tree))) 
          	(if (null? (left-branch tree))
          	    (set-left-branch! record tree)
          	    (adjoin-record-tree! record (left-branch tree))
          	)
          )
          ((> (key record) (key (entry tree)))
          	(if (null? (right-branch tree))
          	    (set-right-branch! record tree)
          	    (adjoin-record-tree! record (right-branch tree))
          	)
          )
    )
)

(define (set-right-branch! record tree)
	(set-cdr! tree (list (left-branch tree) (make-tree record nil nil)))
)

(define (set-left-branch! record tree)
	(set-cdr! tree (list (make-tree record nil nil) (right-branch tree)))
)

(define (make-table)
	
	(let ((local-table (list-to-tree '((1 . martin) (2 . george) (3 . samuel) (4 . john) (5 . tom)))))
		; private methods

		(define (lookup . list-keys)
			(define (iter current-keys current-table)
				(let ((subtable (lookup-tree (car current-keys) current-table)))
					;(debug subtable)
					(if subtable
					    (if (null? (cdr current-keys))
					        (record-value subtable)
					        (iter (cdr current-keys) (cdr subtable))
					    )
					    false
					)
				)
			)
			(iter list-keys local-table)
		)

		(define (insert! value . list-keys)

			(define (form-tree list-keys value)
		  		(let ((first-key (car list-keys)))
		  			(cond ((null? (cdr list-keys)) (make-tree (make-record first-key value) nil nil))
		  			      (else (make-tree (make-record first-key (form-tree (cdr list-keys) value)) nil nil))
		  			)
		  		)
		  	)

		  	(define (iter current-keys current-table)
				(let ((subtable (lookup-tree (car current-keys) current-table)))
					;(debug subtable)
					(if subtable
					    (if (null? (cdr current-keys))
					        (set-cdr! subtable value)
						    (iter (cdr current-keys) (cdr subtable))
					    )
					    (if (null? (cdr current-keys))
					        (adjoin-record-tree! (make-record (car current-keys) value) current-table)
						    (adjoin-record-tree! (make-record (car current-keys) (form-tree (cdr current-keys) value)) current-table)
					    )
					)
				)
			)
			(iter list-keys local-table)

		  	'ok
		)

		; it balances only the main tree, not the subtables
		(define (balance!)
			(define temp (list-to-tree (tree-to-list local-table)))
			(set-car! local-table (car temp))
			(set-cdr! local-table (cdr temp))
			local-table
		)

		; messages

		(define (dispatch m)
		  	(cond ((eq? m 'lookup-proc) lookup)
		  	      ((eq? m 'insert-proc!) insert!)
		  	      ((eq? m 'view) local-table)
		  	      ((eq? m 'balance-tree!) balance!)
		  	      (else (error "Unknown operation: TABLE" m))
		  	)
		)

		dispatch
	)
)

(define (debug x)
	(newline)
	(display "----- ")
	(display x)
	(display " -----")
	(newline)
)

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define balance (operation-table 'balance-tree!))
(define view-table (operation-table 'view))

(put 'sam 6)
(put 'rob 7)
(put 'ned 8)
(put 'dany 9)
(debug view-table)
(balance)

; adding another dimension
(put 'another-level 10 5)
(put 'other 10 6)
(put 'just 10 2)
(debug view-table)