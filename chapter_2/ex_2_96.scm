(define (zeros large)
    (define (iter result count)
        (if (= count large)
            result
            (iter (cons 0 result) (1+ count))
        )
    )
    (iter '(0) 1)
)

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
    (put 'negate '(scheme-number) (lambda (x) (* -1 x)))
    (put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
    'done
)

(install-scheme-number-package)

; ------rational numbers------

(define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        ;(let ((g (gcd n d)))
        ;    (cons (/ n g) (/ d g))
        ;)

        (cons n d)
    )
    (define (add-rat x y)
      (make-rat (add (mul (numer x) (denom y)) (mul (numer y) (denom x))) (mul (denom x) (denom y)))
    )

    (define (sub-rat x y)
     (make-rat (sub (mul (numer x) (denom y)) (mul (numer y) (denom x))) (mul (denom x) (denom y)))
    )

    (define (mul-rat x y)
      (make-rat (mul (numer x) (numer y)) (mul (denom x) (denom y)))
    )

    (define (div-rat x y)
      (make-rat (mul (numer x) (denom y)) (mul (denom x) (numer y)))
    )

    (define (to-real x)
        (mul (div (numer x) (denom x)) 1.0) 
    )

    (define (eq-rat? x y)
        (equ? (mul (numer x) (denom y)) (mul (numer y) (denom x)))
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
        (zero? (numer x))
    )

    (define (negate-rat rational)
        (make-rat (mul -1 (numer rational)) (denom rational))
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
    (put 'negate '(rational) (lambda (rat) (tag (negate-rat rat))))
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

    (define (negate-complex z1)
        (make-from-real-imag (mul -1 (real-part z1)) (mul -1 (imag-part z1)))
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
    (put 'negate '(complex) (lambda (z1) (tag (negate-complex z1))))
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

; term representation
(define (make-term order coeff)
    (list order coeff)
)

(define (order term)
    (car term)
)

(define (coeff term)
    (cadr term)
)

(define (the-empty-termlist)
    nil
)

(define (install-dense-term-list-package)
    (define (empty-termlist? term-list)
        (null? term-list)
    )

    (define (first-term-dense term-list)
        (make-term (-1+ (length term-list)) (car term-list))
    )

    (define (rest-terms-dense term-list)
        (cdr term-list)
    )

    (define (adjoin-term-dense term term-list)
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

    (define (negate-dense term-list)
        (if (empty-termlist? term-list)
            nil
            (let ((first (first-term-dense term-list)))
                (adjoin-term-dense (make-term (order first) (negate (coeff first))) (negate-dense (rest-terms-dense term-list)))
            )
        )
    )

    (define (add-terms-dense L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else 
                (let ((t1 (first-term-dense L1)) (t2 (first-term-dense L2)))
                    (cond ((> (order t1) (order t2)) (adjoin-term-dense t1 (add-terms-dense (rest-terms-dense L1) L2)))
                          ((< (order t1) (order t2)) (adjoin-term-dense t2 (add-terms-dense (rest-terms-dense L2) L1)))
                          (else 
                            (adjoin-term-dense (make-term (order t1) (add (coeff t1) (coeff t2))) (add-terms-dense (rest-terms-dense L1) (rest-terms-dense L2)))
                          )
                    )
                )
              )
        )
    )

    (define (sub-terms-dense L1 L2)
        (add-terms-dense L1 (negate-dense L2))
    )

    (define (mul-terms-dense L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms-dense (mul-term-by-all-terms-dense (first-term-dense L1) L2) (mul-terms-dense (rest-terms-dense L1) L2))
        )
    )

    (define (mul-term-by-all-terms-dense t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term-dense L)))
                (adjoin-term-dense (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2))) (mul-term-by-all-terms-dense t1 (rest-terms-dense L)))
            )
        )    
    )

    (define (div-terms-dense L1 L2)
        (if (empty-termlist? L1)
            (list (the-empty-termlist) (the-empty-termlist))
            (let ((t1 (first-term-dense L1)) (t2 (first-term-dense L2)))
                (if (> (order t2) (order t1))
                    (list (the-empty-termlist) L1)
                    (let ((new-c (div (coeff t1) (coeff t2))) (new-o (- (order t1) (order t2))))
                        (let ((rest-of-result (div-terms-dense (sub-terms-dense L1 (mul-term-by-all-terms-dense (make-term new-o new-c) L2)) L2)))
                            (list (adjoin-term-dense (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))
                        )
                    )
                )
            )
        )
    )

    (define (remainder-terms-dense a b)
        (cadr (div-terms-dense a b))
    )

    (define (pseudoremainder-terms-dense a b)
        (let ((term-a (first-term-dense a)) (term-b (first-term-dense b)))
            (let ((int-factor (expo (coeff term-b) (- (1+ (order term-a)) (order term-b)))))
                (remainder-terms-dense (mul-term-by-all-terms-dense (make-term 0 int-factor) a) b)
            )
        )
    )

    (define (gcd-terms-dense a b)
        (if (empty-termlist? b)
            (gcd-apply-terms-dense a)
            (gcd-terms-dense b (pseudoremainder-terms-dense a b))
        )
    )

    (define (gcd-apply-terms-dense terms)
        (let ((gcd-integer (apply gcd terms)))
            (map (lambda (term) (/ term gcd-integer)) terms)
        )
    )

    ; interface

    (define (tag x) (attach-tag 'dense x))
    (put 'first-term '(dense) first-term-dense)
    (put 'rest-terms '(dense) (lambda (pol) (tag (rest-terms-dense pol))))
    (put 'empty-termlist? '(dense) empty-termlist?)
    (put 'make-terms '(dense) (lambda (terms) (tag terms)))
    (put 'negate '(dense) (lambda (p1) (tag (negate-dense p1))))
    (put 'add-terms '(dense dense) (lambda (term-list1 term-list2) (tag (add-terms-dense term-list1 term-list2))))
    (put 'mul-terms '(dense dense) (lambda (term-list1 term-list2) (tag (mul-terms-dense term-list1 term-list2))))
    (put 'div-terms '(dense dense) (lambda (term-list1 term-list2) (tag (div-terms-dense term-list1 term-list2))))
    (put 'gcd-terms '(dense dense) (lambda (term-list1 term-list2) (tag (gcd-terms-dense term-list1 term-list2))))
)
(install-dense-term-list-package)

(define (make-dense-terms terms)
    ((get 'make-terms '(dense)) terms)
)

(define (install-sparse-term-list-package)
    (define (empty-termlist? term-list)
        (null? term-list)
    )

    (define (first-term-sparse term-list)
        (make-term (car (car term-list)) (cadr (car term-list)))
    )

    (define (rest-terms-sparse term-list)
        (cdr term-list)
    )

    (define (adjoin-term-sparse term term-list)
        (if (zero? (coeff term))
            term-list
            (cons term term-list)
        )
    )

    (define (negate-sparse term-list)
        (if (empty-termlist? term-list)
            nil
            (let ((first (first-term-sparse term-list)))
                (adjoin-term-sparse (make-term (order first) (negate (coeff first))) (negate-sparse (rest-terms-sparse term-list)))
            )
        )
    )

    (define (add-terms-sparse L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else 
                (let ((t1 (first-term-sparse L1)) (t2 (first-term-sparse L2)))
                    (cond ((> (order t1) (order t2)) (adjoin-term-sparse t1 (add-terms-sparse (rest-terms-sparse L1) L2)))
                          ((< (order t1) (order t2)) (adjoin-term-sparse t2 (add-terms-sparse (rest-terms-sparse L2) L1)))
                          (else 
                            (adjoin-term-sparse (make-term (order t1) (add (coeff t1) (coeff t2))) (add-terms-sparse (rest-terms-sparse L1) (rest-terms-sparse L2)))
                          )
                    )
                )
              )
        )
    )

    (define (sub-terms-sparse L1 L2)
        (add-terms-sparse L1 (negate-sparse L2))
    )

    (define (mul-terms-sparse L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms-sparse (mul-term-by-all-terms-sparse (first-term-sparse L1) L2) (mul-terms-sparse (rest-terms-sparse L1) L2))
        )
    )

    (define (mul-term-by-all-terms-sparse t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term-sparse L)))
                (adjoin-term-sparse (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2))) (mul-term-by-all-terms-sparse t1 (rest-terms-sparse L)))
            )
        )    
    )

    (define (div-terms-sparse L1 L2)
        (if (empty-termlist? L1)
            (list (the-empty-termlist) (the-empty-termlist))
            (let ((t1 (first-term-sparse L1)) (t2 (first-term-sparse L2)))
                (if (> (order t2) (order t1))
                    (list (the-empty-termlist) L1)
                    (let ((new-c (div (coeff t1) (coeff t2))) (new-o (- (order t1) (order t2))))
                        (let ((rest-of-result (div-terms-sparse (sub-terms-sparse L1 (mul-term-by-all-terms-sparse (make-term new-o new-c) L2)) L2)))
                            (list (adjoin-term-sparse (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))
                        )
                    )
                )
            )
        )
    )

    ; interface

    (define (tag x) (attach-tag 'sparse x))
    (put 'first-term '(sparse) first-term-sparse)
    (put 'add-terms '(sparse sparse) (lambda (term-list1 term-list2) (tag (add-terms-sparse term-list1 term-list2))))
    (put 'mul-terms '(sparse sparse) (lambda (term-list1 term-list2) (tag (mul-terms-sparse term-list1 term-list2))))
    (put 'rest-terms '(sparse) (lambda (pol) (tag (rest-terms-sparse pol))))
    (put 'empty-termlist? '(sparse) empty-termlist?)
    (put 'make-terms '(sparse) (lambda (terms) (tag terms)))
    (put 'negate '(sparse) (lambda (p1) (tag (negate-sparse p1))))
    (put 'div-terms '(sparse sparse) (lambda (term-list1 term-list2) (tag (div-terms-sparse term-list1 term-list2))))
)
(install-sparse-term-list-package)

(define (make-sparse-terms terms)
    ((get 'make-terms '(sparse)) terms)
)

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

    (define (=zero? p)
        (let ((terms (term-list p)))
            (or (and (= (length terms) 1) (and (zero? (coeff (first-term terms))) (= (order (first-term terms)) 0))) (empty-termlist? terms))
        )
    )

    ; /representation of terms and term list

    (define (negate-poly poly)
        (make-poly (variable poly) (negate (term-list poly)))
    )

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) 
                (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: ADD-POLY" (list p1 p2))
        )
    )

    (define (sub-poly p1 p2)
        (add-poly p1 (negate-poly p2))
    )

    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) 
                (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: MUL-POLY" (list p1 p2))
        )
    )

    (define (div-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) 
                (div-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: DIV-POLY" (list p1 p2))
        )
    )

    (define (gcd-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) 
                (gcd-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: GCD-POLY" (list p1 p2))
        )
    )

    ; interface

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'sub '(polynomial polynomial)
        (lambda (p1 p2) (tag (sub-poly p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'div '(polynomial polynomial) (lambda (p1 p2) (tag (div-poly p1 p2))))
    (put 'make-dense '(polynomial) (lambda (var terms) (tag (make-poly var (make-dense-terms terms)))))
    (put 'make-sparse '(polynomial) (lambda (var terms) (tag (make-poly var (make-sparse-terms terms)))))
    (put 'project '(polynomial) (lambda (p) false))
    (put '=zero? '(polynomial) =zero?)
    (put 'negate '(polynomial) (lambda (p1) (tag (negate-poly p1))))
    (put 'greatest-common-divisor '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
    'done
)

(install-polynomial-package)

(define (make-polynomial-dense var terms)
    ((get 'make-dense '(polynomial)) var terms)
)

(define (make-polynomial-sparse var terms)
    ((get 'make-sparse '(polynomial)) var terms)
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
(define (first-term x) (apply-generic 'first-term x))
(define (rest-terms x) (apply-generic 'rest-terms x))
(define (negate x) (apply-generic 'negate x))
(define (adjoin-term term term-list) (apply-generic 'adjoin-term term term-list))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
(define (add-terms term-list1 term-list2) (apply-generic 'add-terms term-list1 term-list2))
(define (mul-terms term-list1 term-list2) (apply-generic 'mul-terms term-list1 term-list2))
(define (div-terms term-list1 term-list2) (apply-generic 'div-terms term-list1 term-list2))
(define (gcd-terms term-list1 term-list2) (apply-generic 'gcd-terms term-list1 term-list2))
(define (greatest-common-divisor term-list1 term-list2) (apply-generic 'greatest-common-divisor term-list1 term-list2))


(define z1 (make-complex-from-real-imag 2 2))
(define rat1 (make-rational 1 2))
(define z2 (make-complex-from-real-imag rat1 rat1))
(define z3 (make-complex-from-mag-ang (mul (sqrt 2) rat1) 0.79))


(define p1 (make-polynomial-dense 'x '(1 0 0 0 0 -1)))
(define p2 (make-polynomial-dense 'y (list p1)))
(define p3 (make-polynomial-dense 'y (list p1 4 0)))
(define p4 (make-polynomial-sparse 'y '((2 4) (0 -1))))
(define p5 (make-polynomial-sparse 'y (list (list 0 p1))))
(define p6 (make-polynomial-dense 'x '(1 0 -1)))
(define zero-p (make-polynomial-dense 'x '(0)))
(define zero-p-2 (make-polynomial-dense 'x nil))

(define pol1 (make-polynomial-dense 'x '(1 -2 1)))
(define pol2 (make-polynomial-dense 'x '(11 0 7)))
(define pol3 (make-polynomial-dense 'x '(13 5)))

(define pol4 (make-polynomial-dense 'x '(3 5 2)))
(define pol5 (make-polynomial-dense 'x '(2 1)))

(define Q1 (mul pol1 pol2))
(define Q2 (mul pol1 pol3))

(newline)
(display "pol1: ")
(display pol1)
(newline)
(display "GCD: ")
(display (greatest-common-divisor Q1 Q2))
