(define (divides? a b)
	(= (remainder b a) 0)
)

(define (find-divisor n test-divisor)
		(cond ((> (square test-divisor) n) n)
		      ((divides? test-divisor n) test-divisor)
		      (else (find-divisor n (+ test-divisor 1)))
		)
)


(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n) (= (smallest-divisor n) n))


(define (report-prime elapsed-time)
		(display "***")
		(display elapsed-time)
)

(define (start-prime-test n start-time)
		(if (prime? n) 
			(report-prime (- (runtime) start-time))
		)
)

(define (timed-prime-test n)
		(newline)
		(display n)
		(start-prime-test n (runtime))
)

(define (search-for-primes range amount)
		(if (> amount 0)
			(cond ((even? range) (timed-prime-test (+ range 1)) (search-for-primes (+ range 3) (- amount 1)))
			      (else (timed-prime-test range) (search-for-primes (+ range 2) (- amount 1)))
			)
		)
)