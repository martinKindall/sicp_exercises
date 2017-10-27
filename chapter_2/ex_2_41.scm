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
              ((predicate? (car rest)) (iter (append result (list (car rest))) (cdr rest)))
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

(define (unique-pairs n)
    (flatmap 
        (lambda (i) 
            (map
                (lambda (j) 
                    (list i j)
                )
                (enumerate 1 (-1+ i))
            )
        )
        (enumerate 2 n)
    )
)

(define (uniq-triplet n)
  	(flatmap 
        (lambda (i) 
            (map
                (lambda (pair) (cons i pair))
                (unique-pairs (-1+ i))
            )
        )
        (enumerate 3 n)
    )
)

(define (triplet-sum? triplet n)
  	(= (accumulate + 0 triplet) n)
)

(define (triplet-sum-s n s)
  	(filter (lambda (triplet) (triplet-sum? triplet s)) (uniq-triplet n))
)