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

(define (no-more? coins)
  	(null? coins)
)

(define (first-denomination coins)
  	(car coins)
)

(define (except-first-denomination coins)
  	(cdr coins)
)

(define (cc amount coin-values)
	(cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else
			(+ (cc amount
				(except-first-denomination
				coin-values))
			
			   (cc (- amount
			      (first-denomination
					coin-values))
			    coin-values)
			)
		)
	)
)

(define us-coins (list 50 25 10 5 1))
(define us-coins-unordered (list 50 10 25 1 5))

(let ((amount 100))
	(newline)
	(display (cc amount us-coins))
	;; 292
	(newline)
	(display (cc amount us-coins-unordered))
	;; 292
	(newline)
	(display (cc amount (reverse us-coins)))
	;; 292
	(newline)
	(display (cc amount (reverse us-coins-unordered)))
	;; 292
	(newline)
)