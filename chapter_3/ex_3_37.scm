(define (adder a1 a2 sum)
	(define (process-new-value)
	  	(cond ((and (has-value? a1) (has-value? a2))
	  			(set-value! sum
	  						(+ (get-value a1) (get-value a2))
	  						me))
	  	      ((and (has-value? a1) (has-value? sum))
	  	        (set-value! a2
	  						(- (get-value sum) (get-value a1))
	  						me))
	  	      ((and (has-value? a2) (has-value? sum))
	  	        (set-value! a1
	  						(- (get-value sum) (get-value a2))
	  						me))
	  	)
	)

	(define (process-forget-value)
	  	(forget-value! sum me)
	  	(forget-value! a1 me)
	  	(forget-value! a2 me)
	  	(process-new-value)   ; neccesary because some values may have not
	  						  ; been erased, because of informant mismatch
	)

	(define (me request)
	  	(cond ((eq? request 'I-have-a-value) (process-new-value))
	  	      ((eq? request 'I-lost-my-value) (process-forget-value))
	  	      (else (error "Unknown request: ADDER" request))
	  	)
	)

	(connect a1 me)
	(connect a2 me)
	(connect sum me)
	me
)

(define (multiplier m1 m2 product)
	(define (process-new-value)
	  	(cond ((or (and (has-value? m1) (= (get-value m1) 0)) (and (has-value? m2) (= (get-value m2) 0)))
	  			(set-value! product 0 me))
	  	      ((and (has-value? m1) (has-value? m2))
	  	        (set-value! product
	  						(* (get-value m1) (get-value m2))
	  						me))
	  	      ((and (has-value? product) (has-value? m1))
	  	        (set-value! m2
	  						(/ (get-value product) (get-value m1))
	  						me))
	  	      ((and (has-value? product) (has-value? m2))
	  	        (set-value! m1
	  						(/ (get-value product) (get-value m2))
	  						me))
	  	)
	)

	(define (process-forget-value)
	  	(forget-value! product me)
	  	(forget-value! m1 me)
	  	(forget-value! m2 me)
	  	(process-new-value)   ; neccesary because some values may have not
	  						  ; been erased, because of informant mismatch
	)

	(define (me request)
	  	(cond ((eq? request 'I-have-a-value) (process-new-value))
	  	      ((eq? request 'I-lost-my-value) (process-forget-value))
	  	      (else (error "Unknown request: MULTIPLIER" request))
	  	)
	)

	(connect m1 me)
	(connect m2 me)
	(connect product me)
	me
)

(define (constant value connector)
  	(define (me request)
  	  	(error "Unknown request: CONSTANT" request)
  	)
  	(connect connector me)
  	(set-value! connector value me)
  	me
)

(define (probe name connector)
  	(define (print-probe value)
  	  	(newline)(display "Probe: ")(display name)
  	  	(display " = ")(display value)
  	)
  	(define (process-new-value)
  	  	(print-probe (get-value connector))
  	)
  	(define (process-forget-value)
  	  	(print-probe "?")
  	)
  	(define (me request)
  	  	(cond ((eq? request 'I-have-a-value) (process-new-value))
	  	      ((eq? request 'I-lost-my-value) (process-forget-value))
	  	      (else (error "Unknown request: PROBE" request))
  	  	)
  	)
  	(connect connector me)
  	me
)

; n is a value, a and b are connectors
(define (exp-n n a b)
  	(define (process-new-value)
  	  	(if (has-value? b)
  	  	    (if (< (get-value b) 0)
  	  	        (error "root less than 0: EXP-N" (get-value b))

  	  	        (begin 
  	  	        	(set-value! a (expt (get-value b) (/ 1 n)) me)
  	  	        )
  	  	    )
  	  	    (if (has-value? a)
		  	    (set-value! b (expt (get-value a) n) me)
  	       	)
  	  	)
  	)

  	(define (process-forget-value)
  	  	(forget-value! a me)
	  	(forget-value! b me)
	  	(process-new-value)
  	)

  	(define (me request)
		(cond ((eq? request 'I-have-a-value) (process-new-value))
	  	      ((eq? request 'I-lost-my-value) (process-forget-value))
	  	      (else (error "Unknown request: EXP-N" request))
	  	)  	  	
  	)

  	(connect a me)
	(connect b me)
	me
)

(define (make-connector)
  	(let ((value false) (informant false) (constraints '()))
  		(define (set-my-value newval setter)
  		  	(cond ((not (has-value? me)) 
  		  			(set! value newval)
  		  			(set! informant setter)
  		  			(for-each-except 
  		  				setter
  		  				inform-about-value
  		  				constraints
  		  			)
  		  		  )
  		  	      ((not (= value newval)) (error "Contradiction" (list value newval)))
  		  	      (else 'ignored)
 	    	)
  		)

  		(define (forget-my-value retractor)
  		  	(if (eq? retractor informant)
  		  	    (begin (set! informant false)
  		  	    	(for-each-except retractor inform-about-no-value constraints)
  		  	    )
  		  	    'ignored
  		  	)
  		)

  		(define (connect new-constraint)
  		  	(if (not (memq new-constraint constraints))
  		  	    (set! constraints (cons new-constraint constraints))
  		  	)
  		  	(if (has-value? me)
  		  	    (inform-about-value new-constraint)
  		  	)
  		  	'done
  		)

  		(define (me request)
  		  	(cond ((eq? request 'has-value?) (if informant true false))
  		  	      ((eq? request 'value) value)
  		  	      ((eq? request 'set-value!) set-my-value)
  		  	      ((eq? request 'forget) forget-my-value)
  		  	      ((eq? request 'connect) connect)
  		  	      (else (error "Unknown operation: CONNECTOR"))
  		  	)
  		)
  		me
  	)
)

(define (for-each-except exception procedure list)
  	(define (loop items)
  	  	(cond ((null? items) 'done)
  	  	      ((eq? (car items) exception) (loop (cdr items)))
  	  	      (else 
  	  	      	(procedure (car items))
	  	  	    (loop (cdr items)))
  	  	)
  	)
  	(loop list)
)

(define (inform-about-value constraint)
  	(constraint 'I-have-a-value)
)

(define (inform-about-no-value constraint)
  	(constraint 'I-lost-my-value)
)

(define (has-value? connector)
  	(connector 'has-value?)
)

(define (get-value connector)
  	(connector 'value)
)

(define (set-value! connector new-value informant)
  	((connector 'set-value!) new-value informant)
)

(define (forget-value! connector retractor)
  	((connector 'forget) retractor)
)

(define (connect connector new-constraint)
  	((connector 'connect) new-constraint)
)


; expressions

(define (cv c)
	(let ((connector (make-connector)))
		(constant c connector)
		connector
	)
)

(define (c+ x y)
  	(let ((z (make-connector)))
  		(adder x y z)
  		z
  	)
)

(define (c* x y)
  	(let ((z (make-connector)))
  		(multiplier x y z)
  		z
  	)
)

(define (c- x y)
  	(let ((z (make-connector)))
  		(adder x (c* (cv -1) y) z)
  		z
  	)
)

(define (c/ x y)
  	(let ((z (make-connector)) (w (make-connector)))
  		(exp-n -1 y w)
  		(multiplier x w z)
  		z
  	)
)

; celsius to fahren converter

(define (celsius-fahrenheit-converter x)
  	(c+ (c* (c/ (cv 9) (cv 5))
  			x)
  		(cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "C: " C)
(probe "F: " F)

(set-value! C 25 'user)
(forget-value! C 'user)

(set-value! F 77 'user)