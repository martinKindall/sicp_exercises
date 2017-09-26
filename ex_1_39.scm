(define (cont-frac-iter-mod n d k)
	(define (iter i res)
		(if (= i 0)
		    res
		    (iter (-1+ i) (/ (n i) (- (d i) res)))   ;; here is a subtraction
		)
	)
	(iter (-1+ k) (/ (n k) (d k)))
)

(define (tan-cf x k)
	(cont-frac-iter-mod (lambda (i) (if (= i 1) x (square x))) (lambda (i) (- (* 2 i) 1)) k)
)

(define 60_grados_en_radian 1.0471975511965976)

(tan-cf 60_grados_en_radian	100)
;;1.732050807568877 similar to sqrt(3)