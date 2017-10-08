(define (make-interval a b)
  	(cons a b)
)

(define (upper-b inter)
  	(cdr inter)
)

(define (lower-b inter)
  	(car inter)
)

;; abstraction barrier

(define (spans-zero? inter)
    (and (<= (lower-b inter) 0) (>= (upper-b inter) 0))
)

(define (add-interval x y)
  	(make-interval 
  		(+ (lower-b x) (lower-b y))
  		(+ (upper-b x) (upper-b y))
  	)
)

(define (mul-interval x y)
  	(let (
  			(p1 (* (lower-b x) (lower-b y)))
  			(p2 (* (lower-b x) (upper-b y)))
  			(p3 (* (upper-b x) (lower-b y)))
  			(p4 (* (upper-b x) (upper-b y)))
  		)
  		(make-interval
  			(min p1 p2 p3 p4)
  			(max p1 p2 p3 p4)
  		)
  	)
)

(define (div-interval x y)
    (if (spans-zero? y)
        (error "denominator spans zero")
        (mul-interval
          x
          (make-interval
            (/ 1 (upper-b y))
            (/ 1 (lower-b y))
          )
        )
    )
)

(define (sub-interval x y)
    (add-interval x
      (make-interval
        (* -1 (upper-b y))
        (* -1 (lower-b y))
      )
    )
)