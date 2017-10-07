(define tolerance 0.0001)

(define (iterative-improvement good-enough? improve-guess)
  	(define (iter guess)
		(let ((next (improve-guess guess)))
			(if (good-enough? next)
		    	guess
		    	(iter next)
			)
		)
	)
  	(lambda (x) 
  		(iter x)
  	)
  	;; iter   ;; also works as a returned procedure instead calling lambda
)

(define (average x y) 
	(/ (+ x y) 2)
)

(define (sqrt-iter guess x) 
	((iterative-improvement (lambda (y) (< (abs (- (square y) x)) tolerance)) (lambda (y) (average y (/ x y)))) guess)
)

(define (sqrt x) 
	(sqrt-iter 1.0 x)
)

(define (fixed-point f first-guess)
	((iterative-improvement (lambda (y) (< (abs (- (f y) y)) tolerance)) (lambda (y) (f y))) first-guess)
)

(define (average-damp f)
  	(lambda (x) (average x (f x)))
)

(define (sqrt-fix x)
	(fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)