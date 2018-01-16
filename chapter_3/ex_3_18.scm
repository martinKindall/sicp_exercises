; this code is similar to ex_3_17, perhaps one can build an abstraction
; and call it from both procedures

(define (is-cycle? structure)
  	(let ((aux '()))
  		(define (struc-in-aux struc)
  		  	(define (iter currentAux)
  		  		(if (null? currentAux)
	  		  	    false
	  		  	    (if (eq? struc (car currentAux))
	  		  	        true
	  		  	        (iter (cdr currentAux))
	  		  	    )
	  		  	)
  		  	)
  		  	(iter aux)
  		)
  		(define (add-struc-to-aux struc)
  		  	(define temp aux)
  		  	(set! aux (cons struc temp))
  		)
  		(define (iter current)
  			(if (not (pair? current))
  			    false
			    	(if (not (struc-in-aux current))
			    		(begin 
			    			(add-struc-to-aux current)
			    			(iter (cdr current))
			    		)
			    		true
			    	)
  			)
  		)
  		(iter structure)
  	)
)

(define (debug x)
  (newline)
  (display "----- ")
  (display x)
  (display " -----")
  (newline)
)

(define cycle '(a b c))
(define a-list '(a b c))
(set-cdr! (cddr cycle) cycle)

(debug (is-cycle? cycle))
(debug (is-cycle? a-list))