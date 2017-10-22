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

; the following procedure generates a linear recursive process, the previous one a tree-shape process

(define (reverse-deep-iter parameters)
    (define (iter original result)
        (cond ((null? original) result)
              ((pair? (car original)) (iter (cdr original) (append (list (reverse-deep-iter (car original))) result)))
              (else (iter (cdr original) (append (list (car original)) result)))
        )
    )
    (iter parameters ())
)
