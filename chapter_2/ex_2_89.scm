(define (zeros large)
  	(define (iter result count)
  	  	(if (= count large)
  	  	    result
  	  	    (iter (cons 0 result) (1+ count))
  	  	)
  	)
  	(iter '(0) 1)
)



; ---

(define nil '())

(define (filter predicate? sequence)
    (define (iter result rest)
        (cond ((null? rest) result)
              ((predicate? (car rest)) (iter (append result (list (car rest))) (cdr rest)))
              (else (iter result (cdr rest)))
        )
    )
    (iter () sequence)
)

; table module and apply generics

(define table (list)) 

(define (put op type proc) 
    (set! table (append table (list (list op type proc)))) 
)

(define (get op type) 
    (define (search op type t) 
        (cond ((null? t) #f) 
            ((and (equal? (caar t) op) (equal? (cadar t) type)) 
                (caddar t) 
            ) 
            (else (search op type (cdr t)))
        ) 
    ) 
    (search op type table) 
)

(define (attach-tag type-tag contents) 
    (cons type-tag contents) 
) 

(define (type-tag datum)
    (cond ((number? datum) 'scheme-number)
          ((pair? datum) (car datum))
          (else (error "Bad tagged datum: TYPE_TAG" datum))
    )
)

(define (contents datum)
    (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (else (error "Bad tagged datum: CONTENTS " datum))
    )
)

(define (apply-generic op . args)
    (define (noMethodError type-tags)
      (error "No method for these types" (list op type-tags)))

    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags)) (type2 (cadr type-tags))
                            (a1 (car args)) (a2 (cadr args)))
                        (if (or (eq? type1 type2) (eq? type1 'polynomial) (eq? type2 'polynomial))
                            (noMethodError type-tags)
                            (let ((is-greater-t1-t2 (greater type1 type2))
                                    (is-greater-t2-t1 (greater type2 type1)))
                                (cond (is-greater-t1-t2 (apply-generic op a1 (raise a2)))
                                      (is-greater-t2-t1 (apply-generic op (raise a1) a2))
                                      (else (noMethodError type-tags))
                                )
                            )
                        )
                    )
                    (noMethodError type-tags)
                )
            )
        )
    )
)

; ------/table module------

; ------scheme numbers------

(define (install-scheme-number-package)
    (define (to-complex x)
        (make-complex-from-real-imag x 0)
    )

    (define (to-rational x)
        ;(define (iter n d)
        ;    (if (= (round n) n)
        ;        (make-rational (inexact->exact n) d)
        ;        (iter (* 10 n) (* 10 d))
        ;    )
        ;)
        ;(iter x 1)
        false
    )

    (put 'add '(scheme-number scheme-number) +)
    (put 'sub '(scheme-number scheme-number) -)
    (put 'mul '(scheme-number scheme-number) *)
    (put 'div '(scheme-number scheme-number) /)
    (put 'equ? '(scheme-number scheme-number) =)
    (put 'raise '(scheme-number) to-complex)
    (put 'project '(scheme-number) to-rational)
    (put 'cosine '(scheme-number) cos)
    (put 'sine '(scheme-number) sin)
    (put 'atan-new '(scheme-number scheme-number) atan)
    (put 'sqrt-new '(scheme-number) sqrt)
    (put 'square-new '(scheme-number) square)
    (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
    'done
)

(install-scheme-number-package)

; ------rational numbers------

(define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))
        )
    )
    (define (add-rat x y)
      (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))
    )

    (define (sub-rat x y)
     (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y)))
    )

    (define (mul-rat x y)
      (make-rat (* (numer x) (numer y)) (* (denom x) (denom y)))
    )

    (define (div-rat x y)
      (make-rat (* (numer x) (denom y)) (* (denom x) (numer y)))
    )

    (define (to-real x)
        (* (/ (numer x) (denom x)) 1.0) 
    )

    (define (eq-rat? x y)
        (= (* (numer x) (denom y)) (* (numer y) (denom x)))
    )

    (define (cosine x)
        (cos (to-real x))
    )

    (define (sine x)
        (sin (to-real x))
    )

    (define (atan-new x y)
        (atan (to-real x) (to-real y))
    )

    (define (=zero? x)
        (= (numer x) 0)
    )

    ; interface to rest of the system

    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
    (put 'equ? '(rational rational) eq-rat?)
    (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
    (put 'raise '(rational) to-real) ; scheme-numbers are real numbers
    (put 'project '(rational) (lambda (x) false))
    (put 'cosine '(rational) cosine)
    (put 'sine '(rational) sine)
    (put 'atan-new '(rational rational) atan-new)
    (put 'sqrt-new '(rational) (lambda (x) (sqrt (to-real x))))
    (put 'square-new '(rational) (lambda (x) (square (to-real x))))
    (put '=zero? '(rational) =zero?)
    'done
)

(install-rational-package)

(define (make-rational n d)
    ((get 'make 'rational) n d)
)

; ------rectangular numbers------

(define (install-rectangular-package)
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt-new (add (square-new (real-part z)) (square-new (imag-part z))))
    )
    (define (angle z)
        (atan-new (imag-part z) (real-part z))
    )
    (define (make-from-mag-ang r a)
        (cons (mul r (cosine a)) (mul r (sine a)))
    )

    ; interface to the rest

    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag '(rectangular)
        (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put 'make-from-mag-ang '(rectangular)
        (lambda (r a) (tag (make-from-mag-ang r a)))   
    )
    'done
)

; ------polar numbers------

(define (install-polar-package)
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z) (mul (magnitude z) (cosine (angle z))))
    (define (imag-part z) (mul (magnitude z) (sine (angle z))))
    (define (make-from-real-imag x y) 
        (cons (sqrt-new (add (square-new x) (square-new y))) (atan-new y x))
    )

    ; interface to the rest

    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag '(polar)
        (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put 'make-from-mag-ang '(polar)
        (lambda (r a) (tag (make-from-mag-ang r a)))   
    )
    'done
)

(install-polar-package)
(install-rectangular-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; ------complex numbers------

(define (install-complex-package)
    ; imported from rectangular and polar

    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag '(rectangular)) x y)
    )

    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang '(polar)) r a)
    )

    ; internal

    (define (add-complex z1 z2)
        (make-from-real-imag 
            (add (real-part z1) (real-part z2))
            (add (imag-part z1) (imag-part z2))
        )
    )

    (define (sub-complex z1 z2)
        (make-from-real-imag 
            (sub (real-part z1) (real-part z2))
            (sub (imag-part z1) (imag-part z2))
        )
    )

    (define (mul-complex z1 z2)
        (make-from-mag-ang
            (mul (magnitude z1) (magnitude z2))
            (add (angle z1) (angle z2))
        )
    )

    (define (div-complex z1 z2)
        (make-from-mag-ang
            (div (magnitude z1) (magnitude z2))
            (sub (angle z1) (angle z2))
        )
    )

    (define (to-real z)
        (real-part z)
    )

    (define (eq-complex? z1 z2)
        (and (equ? (real-part z1) (real-part z2)) (equ? (imag-part z1) (imag-part z2)))
    )

    (define (=zero? x)
        (eq-complex? x (make-from-real-imag 0 0))
    )

    ; interface

    (define (tag x) (attach-tag 'complex x))
    (put 'add '(complex complex) (lambda (x y) (tag (add-complex x y))))
    (put 'sub '(complex complex) (lambda (x y) (tag (sub-complex x y))))
    (put 'mul '(complex complex) (lambda (x y) (tag (mul-complex x y))))
    (put 'div '(complex complex) (lambda (x y) (tag (div-complex x y))))
    (put 'equ? '(complex complex) eq-complex?)
    (put 'make-from-real-imag '(complex) (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(complex) (lambda (x y) (tag (make-from-mag-ang x y))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'project '(complex) to-real)
    (put '=zero? '(complex) =zero?)
    'done
)

(install-complex-package)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag '(complex)) x y)
)

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang '(complex)) r a)
)

; ------polynomial numbers------

(define (install-polynomial-package)
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    (define (variable? exp)
        (symbol? exp)
    )

    (define (same-variable? x y)
        (and (variable? x) (variable? y) (eq? x y))
    )

    ; representation of terms and term list

    (define (make-term order coeff)
	    (list order coeff)
	)

	(define (order term)
	    (car term)
	)

	(define (coeff term)
	    (cadr term)
	)

	(define (adjoin-term term term-list)
	    (if (zero? (coeff term))
	        term-list
	        (let ((large (length term-list)))
	        	(cond ((= large (order term)) (cons (coeff term) term-list))
	        		((< large (order term)) (cons (coeff term) (append (zeros (- (order term) large)) term-list)))
	        		((> large (order term)) (error "This should not be the case"))
	        	)
	        )
	    )
	)

	(define (first-term term-list)
	  	(make-term (-1+ (length term-list)) (car term-list))
	)

	(define (rest-terms term-list)
	    (cdr term-list)
	)

	(define (the-empty-termlist)
		'()
	)

	(define (empty-termlist? term-list)
		(null? term-list)
	)

    (define (=zero? p)
        (let ((terms (term-list p)))
            (or (and (= (length terms) 1) (and (zero? (coeff (first-term terms))) (= (order (first-term terms)) 0))) (empty-termlist? terms))
        )
    )

    (define (add-terms L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else 
                (let ((t1 (first-term L1)) (t2 (first-term L2)))
                    (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                          ((< (order t1) (order t2)) (adjoin-term t2 (add-terms (rest-terms L2) L1)))
                          (else 
                            (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2))) (add-terms (rest-terms L1) (rest-terms L2)))
                          )
                    )
                )
              )
        )
    )

    (define (mul-terms L1 L2)
    	(newline)
    	(display L1)
    	(display " ")
    	(display L2)
    	(newline)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2) (mul-terms (rest-terms L1) L2))
        )
    )

    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term L)))
                (adjoin-term (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2))) (mul-term-by-all-terms t1 (rest-terms L)))
            )
        )    
    )

    ; /representation of terms and term list

    (define (negate poly)
        (mul-poly poly (make-poly (variable poly) '(-1)))
    )

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) 
                (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: ADD-POLY" (list p1 p2))
        )
    )

    (define (sub-poly p1 p2)
        (add-poly p1 (negate p2))
    )

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) 
                (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: MUL-POLY" (list p1 p2))
        )
    )

    ; interface

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'sub '(polynomial polynomial)
        (lambda (p1 p2) (tag (sub-poly p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'mul '(polynomial scheme-number) (lambda (p1 p2) (tag (mul-poly p1 (make-poly (variable p1) (list p2))))))
    (put 'mul '(scheme-number polynomial) (lambda (p1 p2) (tag (mul-poly p2 (make-poly (variable p2) (list p1))))))
    (put 'make '(polynomial) (lambda (var terms) (tag (make-poly var terms))))
    (put 'project '(polynomial) (lambda (p) false))
    (put '=zero? '(polynomial) =zero?)
    'done
)

(install-polynomial-package)

(define (make-polynomial var terms)
    ((get 'make '(polynomial)) var terms)
)

; ------tower type--------

(define tower-of-types
    '((rational . 1) (scheme-number . 2) (complex . 3))
)

;---------- abstraction barrier ----------

(define (key record)
    (car record)
)
(define (content record)
    (cdr record)
)

;---------- abstraction barrier ----------

(define (lookup given-key list)
    (cond ((null? list) false)
          ((eq? given-key (key (car list))) (content (car list)))
          (else (lookup given-key (cdr list)))
    )
)

;---------- abstraction barrier ----------

(define (greater type1 type2)
    (let ((search-vals (map 
                        (lambda (type) 
                            (let ((result (lookup type tower-of-types)))
                                (if (not result) 
                                    (error "Type value not found: GREATER " type)
                                    result
                                )
                            )
                        ) 
                        (list type1 type2))))
        (> (car search-vals) (cadr search-vals))
    )
)

; main

(define (equ? x y) (apply-generic 'equ? x y))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (drop x) 
    (let ((drop1 (project x)))
        (if drop1 
            (if (equ? x (raise drop1))
                (drop drop1)
                x
            )
            x
        )
    )
)
(define (add x y) (drop (apply-generic 'add x y)))
(define (sub x y) (drop (apply-generic 'sub x y)))
(define (mul x y) (drop (apply-generic 'mul x y)))
(define (div x y) (drop (apply-generic 'div x y)))
(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (atan-new x y) (apply-generic 'atan-new x y))
(define (sqrt-new x) (apply-generic 'sqrt-new x))
(define (square-new x) (apply-generic 'square-new x))
(define (zero? x) (apply-generic '=zero? x))


(define z1 (make-complex-from-real-imag 2 2))
(define rat1 (make-rational 1 2))
(define z2 (make-complex-from-real-imag rat1 rat1))
(define z3 (make-complex-from-mag-ang (mul (sqrt 2) rat1) 0.79))


(define p1 (make-polynomial 'x '(1 0 0 1 0 5)))
(define p2 (make-polynomial 'y (list p1)))
(define p3 (make-polynomial 'y (list p1 4 0)))
(define t2 (make-polynomial 'y '(-1)))
(define zero-p (make-polynomial 'x '(0)))
(define zero-p-2 (make-polynomial 'x '()))