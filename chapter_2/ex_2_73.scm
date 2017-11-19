(define (operator exp)
    (car exp)
)

(define (operands exp)
    (cdr exp)
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

(define (make-exponentiation base exponent)
    (cond ((=number? exponent 1) base)
          ((=number? exponent 0) 1)
          ((and (number? base) (number? exponent)) (expo base exponent))
          (else (list '** base exponent))
    )
)

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp)) (operands exp) var))
    )
)

(define (install-sum)
    ; internal procedures
    (define (addend exp)
        (car exp)
    )

    (define (augend exp)
        (cadr exp)
    )

    (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)
        )
    )

    ; interface to the rest of the system
    ;(define (tag x) (attach-tag '+ x)) ; this is not neccesary, because 
    ; the tag is already o the expression

    (put 'deriv '(+) deriv-sum)

    'done
)

(define (install-prod)
    ; internal procedures

    (define (multiplier exp)
        (car exp)
    )

    (define (multiplicand exp)
        (cadr exp)
    )

    (define (deriv-prod exp var)
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

    ; interface to the rest of the system

    (put 'deriv '(*) deriv-prod)

    'done
)

(define (install-exp)
    ; internal procedures
    (define (base exp)
        (car exp)
    )

    (define (exponent exp)
        (cadr exp)
    )

    (define (deriv-exp exp var)
        (make-product
            (make-product
                (exponent exp)
                (make-exponentiation (base exp) (make-sum (exponent exp) -1))
            )
            (deriv (base exp) var)
        )
    )

    ; interface to the rest of the system

    (put 'deriv '(**) deriv-exp)

    'done
)