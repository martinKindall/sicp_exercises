(define x (list 1 2 3))
(define y (list 4 5 6))

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(define res1 (append x y))
;; guess: (1 2 3 4 5 6)

(define res2 (cons x y))
;; guess: ((1 2 3) 4 5 6)

(define res3 (list x y))
;; guess: ((1 2 3) (4 5 6))


(define list_results (list res1 res2 res3))

(define (for-each proc items)
	(cond ((null? items) true)
	      (else 
	      		(proc (car items))
		    	(for-each proc (cdr items))
	      )
	)
)

(for-each (lambda (x)(newline)(display x)) list_results)