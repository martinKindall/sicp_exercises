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

    ; this procedure returns an alias to the account with another password
    (define (add-pass new-password)
        (lambda (some-password action) (dispatch some-password action new-password))
    )

  	(define (dispatch some-password action stored-password)
  		(if (eq? stored-password some-password)
	  	  	(cond ((eq? action 'withdraw) withdraw)
                  ((eq? action 'deposit) deposit)
	  	  	      ((eq? action 'add-pass) add-pass)
	  	  	      (else (error "Unknown request: MAKE-ACCOUNT" action))
	  	  	)
	  	  	wrong-password-alert
  		)
  	)

    ; the constructor returns the account and compares the password with the initial one
  	(lambda (some-password action) (dispatch some-password action password))
)

(define (make-joint account current-pass new-pass)
    ((account current-pass 'add-pass) new-pass)
)