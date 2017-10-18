(define (iterative-improvement good-enough? improve-guess)
  	(define (iter guess)
		(if (good-enough? guess)
	    	guess
	    	(iter (improve-guess guess))
		)
	)
	iter
)

(define tolerance 0.0001)

(define (fixed-point f first-guess)
	((iterative-improvement (lambda (y) (< (abs (- (f y) y)) tolerance)) f) first-guess)
)

(define dx tolerance)

(define (deriv g)
	(lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)

(define (newton-transform g)
	(lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
	(fixed-point (newton-transform g) guess)
)

(define (sqrt x)
	(newton-method (lambda (y) (- (square y) x)) 1.0)
)

(define (average-damp f)
  	(lambda (x) (/ (+ x (f x)) 2))
)

(define (sqrt-fix x)
	(fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)