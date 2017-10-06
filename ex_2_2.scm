(define (make-segment p1 p2)
	(cons p1 p2)
)

(define (start-segment segment)
  	(car segment)
)

(define (end-segment segment)
  	(cdr segment)
)

(define (make-point x y)
  	(cons x y)
)

(define (x-point point)
  	(car point)
)

(define (y-point point)
  	(cdr point)
)

(define (print-p p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline)
)

(define (average a b)
  	(/ (+ a b) 2)
)

(define (midpoint-segment segment)
	(make-point
		(average (x-point (start-segment segment)) (x-point (end-segment segment)))
		(average (y-point (start-segment segment)) (y-point (end-segment segment)))
	)
)