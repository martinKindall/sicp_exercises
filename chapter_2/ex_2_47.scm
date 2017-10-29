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

; ------------ first frame implementation --------
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

; ------------ second frame implementation --------
(define (make-frame-2 origin edge1 edge2)
  	(cons origin (cons edge1 edge2))
)

(define (origin-frame-2 frame)
  	(car frame)
)

(define (edge1-frame-2 frame)
  	(cadr frame)
)

(define (edge2-frame-2 frame)
  	(cddr frame)
)

(define ori (make-vect 1 1))
(define edge1 (make-vect 5 1))
(define edge2 (make-vect 1 5))

(define frame1 (make-frame ori edge1 edge2))
(define frame2 (make-frame-2 ori edge1 edge2))