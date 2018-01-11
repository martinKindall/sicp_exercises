(define (make-cycle x)
	(set-cdr! (last-pair x) x)
	x
)

(define z (make-cycle '(a b c)))