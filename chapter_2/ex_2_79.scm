;; ordinary numbers package

(put 'equ '(scheme-number scheme-number) =)

;; rational numbers package

(define (eq-rat? x y)
	(= (* (numer x) (denom y)) (* (numer y) (denom x)))
)

; interface

(put 'equ '(rational rational) eq-rat?)


;; complex numbers package

(define (eq-complex? x y)
	(and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))
)

; interface

(put 'equ '(complex complex) eq-complex?)
