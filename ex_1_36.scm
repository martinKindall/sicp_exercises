(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2)
)

(define (show number)
  (newline)
  (display number)
)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
	  (< (abs (- v1 v2)) tolerance)
	)
	(define (try guess)
		(let ((next (f guess)))
			(show next)
			(if (close-enough? guess next)
			    next
			    (try next)
			)
		)
	)
	(try first-guess)
)

;(fixed-point
;	(lambda (x) 
;		(/ (log 1000) (log x))
;	)
;	20
;)

(fixed-point
	(lambda (x) 
		(average x (/ (log 1000) (log x)))
	)
	20
)