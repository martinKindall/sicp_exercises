(define (expmod base exp m)
	(cond ((= exp 0) 1)
	      ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	      (else  (remainder (* base (expmod base (- exp 1) m)) m))
	)
)

(define (fool-charmichael n)
	(define (fool-charmichael-iter n counter)
		(if (< counter n)
		    (if (= (expmod counter n n) counter)
		        (fool-charmichael-iter n (+ counter 1))
		        false
		    )
		    true
		)
	)
	(fool-charmichael-iter n 1)
)