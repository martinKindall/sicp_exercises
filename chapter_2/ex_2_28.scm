(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(define (fringe  lista)
  	(cond ((null? lista) ())
  	      ((not (pair? lista)) (list lista))
  	      (else (append (fringe (car lista)) (fringe (cdr lista))))
  	)
)