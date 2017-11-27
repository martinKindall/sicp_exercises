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
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these tpyes: APPLY-GENERIC " (list op type-tags))
            )
        )
    )
)

; ------/table module------

; ------scheme numbers------

(define (install-scheme-number-package)
    (put 'add '(scheme-number scheme-number) +)
    (put 'sub '(scheme-number scheme-number) -)
    (put 'mul '(scheme-number scheme-number) *)
    (put 'div '(scheme-number scheme-number) /)
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

    ; interface to rest of the system

    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
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
        (sqrt (+ (square (real-part z)) (square (imag-part z))))
    )
    (define (angle z)
        (atan (imag-part z) (real-part z))
    )
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a)))
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
        (lambda (r a) (tag (make-from-real-imag r a)))   
    )
    'done
)

; ------polar numbers------

(define (install-polar-package)
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y) 
        (cons (sqrt (+ (square x) (square y))) (atan y x))
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
        (lambda (r a) (tag (make-from-real-imag r a)))   
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
            (+ (real-part z1) (real-part z2))
            (+ (imag-part z1) (imag-part z2))
        )
    )

    (define (sub-complex z1 z2)
        (make-from-real-imag 
            (- (real-part z1) (real-part z2))
            (- (imag-part z1) (imag-part z2))
        )
    )

    (define (mul-complex z1 z2)
        (make-from-mag-ang
            (* (magnitude z1) (magnitude z2))
            (+ (angle z1) (angle z2))
        )
    )

    (define (div-complex z1 z2)
        (make-from-mag-ang
            (/ (magnitude z1) (magnitude z2))
            (- (angle z1) (angle z2))
        )
    )

    ; interface

    (define (tag x) (attach-tag 'complex x))
    (put 'add '(complex complex) (lambda (x y) (tag (add-complex x y))))
    (put 'sub '(complex complex) (lambda (x y) (tag (sub-complex x y))))
    (put 'mul '(complex complex) (lambda (x y) (tag (mul-complex x y))))
    (put 'div '(complex complex) (lambda (x y) (tag (div-complex x y))))
    (put 'make-from-real-imag '(complex) (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(complex) (lambda (x y) (tag (make-from-mag-ang x y))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    'done
)

(install-complex-package)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag '(complex)) x y)
)

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang '(complex)) r a)
)

; main

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define z1 (make-complex-from-real-imag 3 5))