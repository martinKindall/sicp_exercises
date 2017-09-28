(define (average_3 a b c)
  (/ (+ a b c) 3)
)

(define (compose f g)
	(lambda (x) (f (g x)))
)

(define (repeated f times)
	(if (= times 1)
	    f
	    (compose f (repeated f (-1+ times)))
	)
)

(define dx 0.00001)

(define (smooth f)
	(lambda (x) (average_3 (f x) (f (+ x dx)) (f (- x dx))))
)

(define (smooth-n f n)
	(lambda (x) (((repeated smooth n) f) x))
)
