(define (cons-2 x y)
  	(lambda (m) (m x y))
)

(define (car-2 pair)
  	(pair (lambda (p q) p))
)

(define (cdr-2 pair)
  	(pair (lambda (p q) q))
)