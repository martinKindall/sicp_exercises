(define (monte-carlo trials experiment)
  	(define (iter trials-remaining trials-passed)
  	  	(cond ((= trials-remaining 0) (/ trials-passed trials))
  	  	      ((experiment) (iter (-1+ trials-remaining) (1+ trials-passed)))
  	  	      (else (iter (-1+ trials-remaining) trials-passed))
	  	)
  	)
  	(iter trials 0)
)

(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (random range))
	)
)

; monte carlo integration
(define (estimate-integral predicate x1 x2 y1 y2 trials)
	(define (experiment)
		(let ((rand-x (random-in-range x1 x2)) (rand-y (random-in-range y1 y2)))
			(predicate rand-x rand-y)
		)
	)
	(let ((area (* (- x2 x1) (- y2 y1))))
		(* (monte-carlo trials experiment) area)
	)
)

; circle predicate generator

(define (point-inside-circle? x-cord y-cord radio)
	(lambda (x-guess y-guess)
		(>= (square radio) (+ (square (- x-guess x-cord)) (square (- y-guess y-cord))))
	)
)

; circle located at (0, 0) and radio 1

(define unit-circle (point-inside-circle? 0 0 1))
(newline)
(display "We can estimate pi as: ")
(display (estimate-integral unit-circle -1.0 1.0 -1.0 1.0 100000))
(newline)