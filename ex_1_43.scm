(define (compose f g)
	(lambda (x) (f (g x)))
)

(define (repeated f times)
	(if (= times 1)
	    f
	    (compose f (repeated f (-1+ times)))
	)
)

((repeated square 2) 5)