; integer package

(define (toRat x)
    (attach-tag 'rational (make-rational x 1))
)

(put 'raise 'integer toRat)

; rational package

(define (toReal x)
    (attach-tag 'real (/ (numer x) (denom x)))
)

(put 'raise 'rational toReal)

; real package

(define (toComplex x)
    (attach-tag 'complex (make-complex-from-real-imag x 0))
)

(put 'raise 'real toComplex)