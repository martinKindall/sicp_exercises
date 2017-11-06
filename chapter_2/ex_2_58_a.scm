(define (even? x)
	(= (remainder x 2) 0)
)

(define (expo b n)
	(define (exp-iter b a n)
		(cond ((= n 0) a)
		      ((even? n) (exp-iter (square b) a (/ n 2)))
		      (else (exp-iter b (* a b) (- n 1)))
		)
	)
	(exp-iter b 1 n)
)

(define (accumulate op initial sequence)
    (cond ((null? sequence) initial)
          (else (op (car sequence) (accumulate op initial (cdr sequence))))
    )
)

(define (=number? exp number)
  	(and (number? exp) (= exp number))
)

(define (variable? exp)
  	(symbol? exp)
)

(define (same-variable? x y)
  	(and (variable? x) (variable? y) (eq? x y))
)

(define (make-sum exp1 exp2)
    (cond ((=number? exp1 0) exp2)
          ((=number? exp2 0) exp1)
          ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
          (else (list exp1 '+ exp2))
    )    
)

(define (sum? exp)
    (and (pair? exp) (eq? '+ (cadr exp)))
)

(define (addend exp)
    (car exp)
)

(define (augend exp)
    (caddr exp)
)

(define (make-product exp1 exp2)
    (cond ((or (=number? exp1 0) (=number? exp2 0)) 0)
          ((=number? exp1 1) exp2)
          ((=number? exp2 1) exp1)
          ((and (number? exp1) (number? exp2)) (* exp1 exp2))
          (else (list exp1 '* exp2))
    )
)

(define (product? exp)
    (and (pair? exp) (eq? '* (cadr exp)))
)

(define (multiplier exp)
    (car exp)
)

(define (multiplicand exp)
    (caddr exp)
)

;----------------------------------------------------------------

(define (deriv exp var)
  	(cond ((number? exp) 0)
  	      ((variable? exp) (if (same-variable? exp var) 1 0))
  	      ((sum? exp) (make-sum (deriv (addend exp) var)
  	      						(deriv (augend exp) var)
  	      			  )
  	      )
  	      ((product? exp) 
  	      	(make-sum
  	      		(make-product
  	      			(multiplier exp)
  	      			(deriv (multiplicand exp) var)
  	      		)
  	      		(make-product
  	      			(multiplicand exp)
  	      			(deriv (multiplier exp) var)
  	      		)
  	      	)
  	      )
  	      (else (error "Unknown expression type: DERIV" exp))
  	)
)