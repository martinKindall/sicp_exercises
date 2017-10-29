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

; ------------ frame implementation --------
(define (make-frame origin edge1 edge2)
  	(list origin edge1 edge2)
)

(define (origin-frame frame)
  	(car frame)
)

(define (edge1-frame frame)
  	(cadr frame)
)

(define (edge2-frame frame)
  	(caddr frame)
)

(define (frame-coord-map frame)
  	(lambda (v) 
  		(add-vect
  			(origin-frame frame)
  			(add-vect
  				(scale-vect (xcor-vect v) (edge1-frame frame))
  				(scale-vect (ycor-vect v) (edge2-frame frame))
  			)
  		)
  	)
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

;------------ segment painter ---------------

(define (segments->painter segment-list)
  	(lambda (frame) 
  		(for-each
  			(lambda (segment) 
  				(draw-line 
  					((frame-coord-map frame) (start-segment segment))
  					((frame-coord-map frame) (end-segment segment))
  				)
  			)
  			segment-list
  		)
  	)
)