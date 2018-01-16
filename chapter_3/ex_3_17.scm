; the idea behind count-pairs is to save the pointers to the pairs in the list structure in an auxiliary list,
; so we can compare later with the traversed structure and not count twice any pair

(define (count-pairs structure)
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
  			;(debug aux)
  			(if (not (pair? current))
  			    0
  			    (+ (iter (car current)) (iter (cdr current)) 
  			    	(if (not (struc-in-aux current))
  			    		(begin 
  			    			(add-struc-to-aux current)
  			    			1
  			    		)
  			    		0
  			    	)
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

; --------- case return 4 ---------
(define y '(b))
(define x (cons 'a y))


; z2 is a list of three pairs
(define z2 (cons x y))

(count-pairs z2)
; now it returns 3

; --------- case return 7 ---------
(define w (cons y y))
(define z3 (cons w w))

(count-pairs z3)
; now it returns 3