(define (square x) (* x x))

(define (even? x)
	(= (remainder x 2) 0)
)

(define (expo b n)
	(define (exp-iter c b a n)
		(cond ((= n 0) 1)
			  ((= n 1) (* a b))
		      ((even? n) (exp-iter c (square b) a (/ n 2)))
		      (else (exp-iter c b (* a c) (- n 1)))
		)
	)
	(exp-iter b b 1 n)
)