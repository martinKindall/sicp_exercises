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


; complex constraints

; squarer
; int -> int
; returns a² = a*a

(define (squarer a b)
	(multiplier a a b)
)

(define A (make-connector))
(define B (make-connector))

(squarer A B)

(probe "A: " A)
(probe "B: " B)

; this works
(set-value! A 10 'user)

;Probe: A:  = 10
;Probe: B:  = 100

(forget-value! A 'user)

; this doesn't work
(set-value! B 100 'user)

; A remains without value, because of the 'cond' in the multiplier definition