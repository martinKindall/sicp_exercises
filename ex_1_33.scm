(define (square x)
  (* x x)
)

(define (expmod base exp m)
	(cond ((= exp 0) 1)
	      ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	      (else  (remainder (* base (expmod base (- exp 1) m)) m))
	)
)

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a)
	)
	(try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
	(cond ((= times 0) true)
	      ((fermat-test n) (fast-prime? n (- times 1)))
	      (else false)
	)
)

(define (prime? x)
  (fast-prime? x 100)
)

(define (filtered-accumulate combiner null-value filter term a next b)
	(define (iter a result)
		(cond ((> a b) result)
		      ((filter a) (iter (next a) (combiner result (term a))))
		      (else (iter (next a) result))
		)
	)
	(iter a null-value)
)

(define (sum-prime-squared a b)
  (filtered-accumulate + 0 prime? square a 1+ b)
)

(define (always-true a)
  true
)

(define (sum term a next b)
  (filtered-accumulate + 0 always-true term a next b)
)

(define (identity x)
  x
)

(define (gcd a b)
	(if (= b 0)
	    a
	    (gcd b (remainder a b))
	)
)

(define (rel-prime? a n)
  (= (gcd n a) 1)
)

(define (prod-rel-primes n)
	(define (rel-prime-aux? x)
		(rel-prime? x n)
	)
	(filtered-accumulate * 1 rel-prime-aux? identity 1 1+ (- n 1))
)