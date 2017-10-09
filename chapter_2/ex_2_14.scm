(define (make-interval a b)
  	(cons a b)
)

(define (make-center-width c w)
    (make-interval (- c w) (+ c w))
)

(define (average a b)
    (/ (+ a b) 2.0)
)

(define (center inter)
    (average (lower-b inter) (upper-b inter))
)

(define (width inter)
    (/ (- (upper-b inter) (lower-b inter)) 2.0)
)

(define (make-center-percent c percent)
    (make-interval (- c (* c (/ percent 100.0))) (+ c (* c (/ percent 100.0))))
)

(define (percent inter)
    (* (/ (width inter) (center inter)) 100.0)
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
            (/ 1.0 (upper-b y))
            (/ 1.0 (lower-b y))
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

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2) (add-interval r1 r2))
)

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
      (div-interval one (add-interval (div-interval one r1) (div-interval one r2)))
    )
)