(define (make-interval a width)
  	(cons a width)
)

(define (amount inter)
    (car inter)
)

(define (width inter)
    (cdr inter)
)

(define (upper-b inter)
    (+ (amount inter) (width inter))
)

(define (lower-b inter)
  	(- (amount inter) (width inter))
)

;; abstraction barrier

(define (add-interval-v2 x y)
    (make-interval 
      (+ (amount x) (amount y))
      (+ (width x) (width y))   ;; the new width is the sum of the widths
    )
)

(define (mul-interval-v2 x y)
  	(make-interval 
  		(+ (* (amount x) (amount y)) (* (width x) (width y)))
  		(+ (* (amount y) (width x)) (* (amount x) (width y)))   ;; mult is now a function of the widths and the amounts too
  	)
)


