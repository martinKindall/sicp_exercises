(define (square-list items)
	(if (null? items)
		'()
		(cons (square (car items)) (square-list (cdr items)))
	)
)

(define (map proc items)
  	(if (null? items)
  	    '()
  	    (cons (proc (car items)) (map proc (cdr items)))
  	)
)

(define (square-list-v2 items)
	(map (lambda (x) (square x)) items)
)