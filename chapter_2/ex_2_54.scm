(define (equal?-custom list1 list2)
	(cond ((null? list1) (null? list2))
	      ((not (pair? (car list1))) (and (eq? (car list1) (car list2)) (equal?-custom (cdr list1) (cdr list2))))
	      (else (let ((eq1 (equal?-custom (car list1) (car list2))))
      				(and eq1 (equal?-custom (cdr list1) (cdr list2)))
      			)
	  	  )
	)
)

(define res1 (equal?-custom (list '(a c) '(e b)) (list '(a c) '(e b))))
(define res2 (equal?-custom '(this is a list) '(this (is a) list)))
(define res3 (equal?-custom '(this is a list) '(this is a list)))
(define res4 (equal?-custom '(this is a list) '(this is a list yes)))

(define results (list res1 res2 res3 res4))

(for-each (lambda (item) (newline)(display item)) results)