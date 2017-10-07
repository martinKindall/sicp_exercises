(define (identity x)
  x
)

(define (sum f a next b)
	(define (iter a result)
		(if (> a b)
		    result
		    (iter (next a) (+ result (f a)))
		)
	)
	(iter a 0)
)