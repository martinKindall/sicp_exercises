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
  	      (else (list '+ exp1 exp2))
  	)
)

(define (make-product exp1 exp2)
  	(cond ((or (=number? exp1 0) (=number? exp2 0)) 0)
  	      ((=number? exp1 1) exp2)
  	      ((=number? exp2 1) exp1)
  	      ((and (number? exp1) (number? exp2)) (* exp1 exp2))
  	      (else (list '* exp1 exp2))
  	)
)

(define (sum? exp)
  	(and (pair? exp) (eq? '+ (car exp)))
)

(define (addend exp)
  	(cadr exp)
)

(define (augend exp)
  	(caddr exp)
)

(define (product? exp)
  	(and (pair? exp) (eq? '* (car exp)))
)

(define (multiplier exp)
  	(cadr exp)
)

(define (multiplicand exp)
  	(caddr exp)
)

(define (exponentiation? exp)
  	(and (pair? exp) (eq? '** (car exp)))
)

(define (base exp)
  	(cadr exp)
)

(define (exponent exp)
  	(caddr exp)
)

(define (make-exponentiation base exponent)
  	(cond ((=number? exponent 1) base)
  	      ((=number? exponent 0) 1)
  	      ((and (number? base) (number? exponent)) (expo base exponent))
  	      (else (list '** base exponent))
  	)
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
  	      ((exponentiation? exp)
  	      	(make-product
  	      		(make-product
  	      			(exponent exp)
  	      			(make-exponentiation (base exp) (make-sum (exponent exp) -1))
  	      		)
  	      		(deriv (base exp) var)
  	      	)
  	      )
  	      (else (error "Unknown expression type: DERIV" exp))
  	)
)