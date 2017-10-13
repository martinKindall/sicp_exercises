(define list1 (list 1 3 (list 5 7) 9))

(define list2 (list (list 7)))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))


(define res1 (car (cdaddr list1)))

(define res2 (caar list2))

(define res3 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3)))))))))))))


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