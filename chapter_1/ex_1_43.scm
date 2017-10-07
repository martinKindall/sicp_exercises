(define (compose f g)
	(lambda (x) (f (g x)))
)

(define (repeated f times)
	(if (= times 1)
	    f
	    (compose f (repeated f (-1+ times)))
	)
)

(define (repeated-iter f times)
	(define (iter count res)
		(if (= count times)
		    res
		    (iter (1+ count) (compose f res))
		)
	)
	(iter 1 f)
)

;((repeated square 2) 5)

((repeated-iter square 2) 5)