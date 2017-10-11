(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(define (isParity? parity)
  	(lambda (x) (if (even? parity)
  	    				(even? x)
  	    			 	(not (even? x))
  	    		  )
  				)
)

(define (same-parity parity . numbers)
	(let ((checkParity? (isParity? parity)))
		(define (iter numbers)
	  	(if (null? numbers)
		  	    numbers
		  	    (if (checkParity? (car numbers))
		  	        (append (list (car numbers)) (iter (cdr numbers)))
		  	        (iter (cdr numbers))
		  	    )
		  	)
		)
		(append (list parity) (iter numbers))
	)
)


