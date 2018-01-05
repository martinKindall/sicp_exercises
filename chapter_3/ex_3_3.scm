(define (make-account balance password)
  	(define (withdraw value)
  	  	(if (>= balance value)
  	  	    (begin (set! balance (- balance value)) balance)
  	  	    "not enough money in the account"
  	  	)
  	)
  	(define (deposit value)
  		(set! balance (+ balance value))
  		balance  	
  	)
  	(define (wrong-password-alert value)
  		"wrong-password"
  	)
  	(define (dispatch some-password action)
  		(if (eq? password some-password)
	  	  	(cond ((eq? action 'withdraw) withdraw)
	  	  	      ((eq? action 'deposit) deposit)
	  	  	      (else (error "Unknown request: MAKE-ACCOUNT" action))
	  	  	)
	  	  	wrong-password-alert
  		)
  	)
  	dispatch
)