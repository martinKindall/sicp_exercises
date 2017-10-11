(define (square-list items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				(cons answer (square (car things)))
				;; the problem is here in the cons,
				;; the list is being constructed with nil
				;; at the beginning and also the 'tail'
				;; of the list is not a list anymore, it is
				;; a number
			)
		)
	)
	(iter items '())
)