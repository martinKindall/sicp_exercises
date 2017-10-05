(define (gcd a b)
	(if (= b 0)
	    a
	    (gcd b (remainder a b))
	)
)

;; basic implementation
;(define (make-rat numer denom)
;    (cons numer denom)
;)


;; reduced fraction and sign normalization 
(define (make-rat numer denom)
  (let ((g (abs (gcd numer denom))))
    (cond ((or (and (< numer 0) (< denom 0)) (and (< denom 0) (> numer 0))) (cons (/ (* numer -1) g) (/ (* denom -1) g)))
          (else (cons (/ numer g) (/ denom g)))
    )
  )
)

(define (numer rat)
  (car rat)
)
(define (denom rat)
  (cdr rat)
)

;; here is an abstraction barrier: between the implementation of constructor and selector (the way we represent a rational number) and 
;; the implementation of the operations on rational numbers

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

(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (denom x) (numer y)))  
)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline)
)
