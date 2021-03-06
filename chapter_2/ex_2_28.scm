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

; the following procedure generates a linear recursive process, the previous one a tree-shape process

(define (fringe-iter parameters)
  	(define (iter original result)
  	  	(cond ((null? original) result)
  	  	      ((pair? original) (iter (cdr original) (append result (fringe-iter (car original)))))
  	  	      (else (list original))
  	  	)
  	)
  	(iter parameters ())
)