(define (accumulate op initial sequence)
    (cond ((null? sequence) initial)
          (else (op (car sequence) (accumulate op initial (cdr sequence))))
    )
)

(define (eq-list? aList)
    (let ((first (car aList)))
        (accumulate (lambda (x y) (and x y)) true (map (lambda (item) (eq? first item)) (cdr aList)))
    )
)

(define (identity x) x)

(define (filter predicate? sequence)
    (define (iter result rest)
        (cond ((null? rest) result)
              ((predicate? (car rest)) (iter (append result (list (car rest))) (cdr rest)))
              (else (iter result (cdr rest)))
        )
    )
    (iter () sequence)
)

(define (apply-generic op . args)
    (define (noMethodError type-tags)
       (error "No method for these types" (list op type-tags)))


  	(let ((type-tags (map type-tag args)))
  		(let ((proc (get op type-tags)))
  			(if proc
  			    (apply proc (map contents args))
  			    (if (eq-list? type-tags)
                (noMethodError type-tags)
                (let ((listOfCoercions (map 
                                        (lambda (type)  
                                            (map 
                                                (lambda (item) 
                                                    (if (eq? item type)
                                                        identity
                                                        (get-coercion item type)
                                                    )
                                                ) 
                                                type-tags
                                            )
                                        ) 
                                        type-tags)))
                    (let ((validCoercions (filter 
                                            (lambda (aList) 
                                                (accumulate (lambda (x y) (and x y)) true aList)
                                            ) 
                                            listOfCoercions)))
                        (if (null? validCoercions)
                            (noMethodError type-tags)
                            (apply apply-generic (cons op (map (lambda (proc arg) (proc arg)) (car validCoercions) args)))
                        )
                    )
                )
            )
  			)
  		)
  	)
)