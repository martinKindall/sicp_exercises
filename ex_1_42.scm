(define (compose f g)
	(lambda (x) (f (g x)))
)

((compose square 1+) 6)