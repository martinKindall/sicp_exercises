; ------------ vector implementation --------

(define (make-vect x y)
	(cons x y)  	
)

(define (xcor-vect v)
  	(car v)
)

(define (ycor-vect v)
  	(cdr v)
)

; ------------ segments implementation --------

(define (make-segment v1 v2)
  	(cons v1 v2)
)

(define (start-seg seg)
  	(car seg)
)

(define (end-seg seg)
  	(cdr seg)
)