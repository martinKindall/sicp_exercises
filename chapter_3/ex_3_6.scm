(define rand 
	(let ((x random-init))

		(define (generate)
			(set-x (rand-update x))
		)
		
		(define (reset new-value)
			(set-x new-value)
		)

		(define (set-x value)
		  	(set! x value)
		  	x
		)

		(lambda (command)
			(cond ((eq? command 'generate) generate)
			      ((eq? command 'reset) reset)
			      (else (error "unknown command: RAND" command))
			)
		)
	)
)