; --------------- abstraction barier ---------------

(define (make-vect x y)
	(cons x y)  	
)

(define (xcor-vect v)
  	(car v)
)

(define (ycor-vect v)
  	(cdr v)
)

; --------------- abstraction barier ---------------

(define (op-vect op v1 v2)
  	(make-vect (op (xcor-vect v1) (xcor-vect v2)) (op (ycor-vect v1) (ycor-vect v2)))
)

(define (add-vect v1 v2)
	(op-vect + v1 v2)
)

(define (sub-vect v1 v2)
  	(op-vect - v1 v2)
)

(define (scale-vect param v)
  	(op-vect * (make-vect param param) v)
)
