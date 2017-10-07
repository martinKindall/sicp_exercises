(define (toCube x)
  (* x (square x))
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
	  (< (abs (- v1 v2)) tolerance)
	)
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
			    next
			    (try next)
			)
		)
	)
	(try first-guess)
)

(define dx tolerance)

(define (deriv g)
	(lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)

(define (newton-transform g)
	(lambda (x) (- x (/ (g x) ((deriv g) x)))
	)
)

(define (newton-method g guess)
	(fixed-point (newton-transform g) guess)
)

(define (cube a b c)
	(lambda (x) (+ (toCube x) (* a (square x)) (* b x) c))
)

(newton-method (cube 3 -2.4 6) 1)