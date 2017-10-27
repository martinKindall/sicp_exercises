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

(define (enumerate a b)
  	(define (iter current result)
  		(if (> current b)
  		    result
  		    (iter (1+ current) (append result (list current)))
  		)
  	)
  	(iter a ())
)

(define (filter predicate? sequence)
	(define (iter result rest)
	  	(cond ((null? rest) result)
	  	      ((predicate? (car rest)) (iter (append result (car rest)) (cdr rest)))
	  	      (else (iter result (cdr rest)))
	  	)
	)
	(iter () sequence)
)

(define (accumulate op initial sequence)
  	(cond ((null? sequence) initial)
  	      (else (op (car sequence) (accumulate op initial (cdr sequence))))
  	)
)

(define (map proc sequence)
  	(accumulate 
  		(lambda (x y)
  			(cons (proc x) y)
	  	) 
    	'()
    	sequence	
	)
)

(define (append seq1 seq2)
  	(accumulate cons seq2 seq1)
)

(define (flatmap proc seq)
  	(accumulate append '() (map proc seq))
)

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
  	(list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (prime-sum-pairs n)
	(map
		make-pair-sum
		(filter
			prime-sum?
		  	(flatmap 
		  		(lambda (i) 
		  			(map 
		  				(lambda (j) 
		  					(list i j)
		  				)
		  				(enumerate 1 (- i 1))
		  			)
		  		)
		  		(enumerate 1 n)
		  	)
		)
	)
)