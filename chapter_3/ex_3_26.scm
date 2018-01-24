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

(define (record-value record)
  	(cadr record)
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

(define (make-table)
	
	(let ((local-table (list-to-tree '((1 martin) (2 george) (3 samuel) (4 john) (5 tom)))))
		; private methods

		(define (lookup . list-keys)
			(define (iter current-keys current-table)
				(let ((subtable (lookup-tree (car current-keys) current-table)))
					;(debug subtable)
					(if subtable
					    (if (null? (cdr current-keys))
					        (record-value subtable)
					        (iter (cdr current-keys) (record-value subtable))
					    )
					    false
					)
				)
			)
			(iter list-keys local-table)
		)

		(define (insert! value . list-keys)

		  	(define (form-list list-keys value)
		  		(let ((first-key (car list-keys)))
		  			(cond ((null? (cdr list-keys)) (cons first-key value))
		  			      (else (list first-key (form-list (cdr list-keys) value)))
		  			)
		  		)
		  	)

		  	(define (iter current-keys current-table)
				(let ((subtable (assoc (car current-keys) (cdr current-table))))
					;(debug subtable)
					(if subtable
					    (if (null? (cdr current-keys))
					        (set-cdr! subtable value)
						    (iter (cdr current-keys) subtable)
					    )
					    (if (null? (cdr current-keys))
					        (set-cdr! current-table (cons (cons (car current-keys) value) (cdr current-table)))
						    (set-cdr! current-table (cons (form-list current-keys value) (cdr current-table)))
					    )
					)
				)
			)
			(iter list-keys local-table)

		  	'ok
		)

		; messages

		(define (dispatch m)
		  	(cond ((eq? m 'lookup-proc) lookup)
		  	      ((eq? m 'insert-proc!) insert!)
		  	      ((eq? m 'view) local-table)
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
(define view-table (operation-table 'view))

;(put 'tamarugo 'arboles 'chile)
;(put 'mango 'arboles 'peru)
;(put 'banano 'arboles 'colombia)

;(put 'sqrt 'math 'functions 'square-root)
;(put 50 'math 'operations '-)
;(put 43 'math 'operations '+)

;view-table