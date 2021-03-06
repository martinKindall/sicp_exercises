(define (cont-frac-iter n d k)
	(define (iter i res)
		(if (= i 0)
		    res
		    (iter (-1+ i) (/ (n i) (+ (d i) res)))
		)
	)
	(iter (-1+ k) (/ (n k) (d k)))
)

(define (serie x)
	(cond ((= x 1) 1)
	      ((= x 2) 2)
	      ((= x 3) 1)
	      ((= x 4) 1)
	      ((and (= (serie (- x 1)) 1) (= (serie (- x 2)) 1)) (+ 2 (serie (- x 3))))
	      (else 1)
	)
)


;(let ((euler-2 (cont-frac-iter (lambda (x) 1.0) serie 10)))
;	(+ euler-2 2)
;)

(let ((euler-2 
		(cont-frac-iter 
			(lambda (x) 1.0) 
			(lambda (x) 
				(cond ((= x 1) 1)
				      ((= x 2) 2)
				      ((= (remainder (- x 2) 3) 0) (- x (/ (- x 2) 3)))
				      (else 1)
				)
			) 
			10
		)
	  )
	)
	(+ euler-2 2)
)