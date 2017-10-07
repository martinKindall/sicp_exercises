(define (cont-frac n d k)
	(define (recursive i)
		(if (= i k)
		    (/ (n k) (d k))
		    (/ (n i) (+ (d i) (recursive (1+ i))))
		)
	)
	(recursive 1)
)

(define (cont-frac-iter n d k)
	(define (iter i res)
		(if (= i 0)
		    res
		    (iter (-1+ i) (/ (n i) (+ (d i) res)))
		)
	)
	(iter (-1+ k) (/ (n k) (d k)))
)


;(let ((1_phi (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 12)))
;	(/ 1 1_phi)
;)

(let ((1_phi (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) 12)))
	(/ 1 1_phi)
)

