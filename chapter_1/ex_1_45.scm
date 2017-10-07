(define (square x) (* x x))

(define (even? x)
	(= (remainder x 2) 0)
)

(define (expo b n)
	(define (exp-iter b a n)
		(cond ((= n 0) a)
		      ((even? n) (exp-iter (square b) a (/ n 2)))
		      (else (exp-iter b (* a b) (- n 1)))
		)
	)
	(exp-iter b 1 n)
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

(define (compose f g)
	(lambda (x) (f (g x)))
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

(define (average x y)
  	(/ (+ x y) 2)
)

(define (average-damp f)
  	(lambda (x) (average x (f x)))
)

(define (log_2 x)
	(/ (log x) (log 2))
)

(define (n-root x n)
	(fixed-point ((repeated-iter average-damp (floor (log_2 n))) (lambda (y) (/ x (expo y (- n 1))))) 1.0)
)