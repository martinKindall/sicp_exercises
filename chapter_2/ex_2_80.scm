;; ordinary numbers package

(put 'equ '(scheme-number scheme-number) =)
(put '=zero? '(scheme-number) (lambda (x) (= x 0)))

;; rational numbers package

(define (eq-rat? x y)
	(= (* (numer x) (denom y)) (* (numer y) (denom x)))
)

(define (=zero? x)
	(= (numer x) 0)
)

; interface

(put 'equ '(rational rational) eq-rat?)
(put '=zero? '(rational) =zero?)


;; complex numbers package

(define (eq-complex? x y)
	(and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))
)

(define (=zero? x)
	(eq-complex? x (make-from-real-imag 0 0))
)

; interface

(put 'equ '(complex complex) eq-complex?)
(put '=zero? '(complex) =zero?)
