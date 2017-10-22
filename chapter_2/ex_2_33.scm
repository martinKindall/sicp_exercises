(define (accumulate op initial sequence)
  	(cond ((null? sequence) initial)
  	      (else (op (car sequence) (accumulate op initial (cdr sequence))))
  	)
)

(define (map proc sequence)
  	(accumulate (lambda (x y) ; x would be (car sequence) and y (accumulate op initial (cdr sequence))
  		(cons (proc x) y)
  	) 
    '()
    sequence
	)
)

(define (append seq1 seq2)
  	(accumulate cons seq2 seq1)
)

(define (length sequence)
  	(accumulate (lambda (x y) (+ 1 y)) 0 sequence)
)

(define (reverse lista)
    (if (null? lista)
        lista
        (append (reverse (cdr lista)) (list (car lista)))
    )
)

; for-each in terms of high order procedures, maybe not efficient

(define (for-each proc sequence)
  	(accumulate (lambda (x y) (proc x)) true (reverse sequence))
)

(for-each (lambda (x)(newline)(display x)) (list 1 2 3 4 5))