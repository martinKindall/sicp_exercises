(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(define (reverse lista)
    (if (null? lista)
        lista
        (append (reverse (cdr lista)) (list (car lista)))
    )
)

(define (reverse-deep lista)
    (cond ((null? lista) lista)
          ((pair? (car lista)) (append (reverse-deep (cdr lista)) (list (reverse-deep (car lista)))))
          (else (append (reverse-deep (cdr lista)) (list (car lista))))
	)
)
