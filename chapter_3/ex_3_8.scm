(define func 
	(let ((init-d 1))
		(lambda (delta) 
			(if (= delta 0)
			    (begin 
			    	(set! init-d delta)
			    	0
			    )
			    (* delta init-d)
			)
		)
	)
)

(newline)
(display (+ (func 0) (func 1)))
(newline)
(display (+ (func 1) (func 0)))
(newline)