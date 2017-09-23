(define (identity x)
  x
)

(define (pitatoria f a next b)
  (if (> a b)
      1
      (* (f a) (pitatoria f (next a) next b))
  )
)

(define (factorial-new x)
  (pitatoria identity 1 1+ x)
)

(define (pi-series n)
  (define (f-aux x)
    (if (even? x)
        (/ (+ x 2.0) (+ x 3.0))
        (/ (+ x 3.0) (+ x 2.0))
    )
  )
  (pitatoria f-aux 0.0 1+ n)
)