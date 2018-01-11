(define (mystery x)
	(define (loop x y)
	  	(if (null? x)
	  	    y
	  	    (let ((temp (cdr x)))
	  	    	(set-cdr! x y)
	  	    	(loop temp x)
	  	    )
	  	)
	)
	(loop x '())
)

(define v '(a b c d))

(newline)
(display (mystery v))
; expected (d c b a), just as a reverse
(newline)
(display v)
; expected (a)
(newline)

(define (reverse! x)
	(mystery x)
)