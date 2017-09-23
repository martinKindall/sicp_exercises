(define (identity x)
  x
)

;; pitatoria recursiva
(define (pitatoria f a next b)
  (if (> a b)
      1
      (* (f a) (pitatoria f (next a) next b))
  )
)

;; pitatoria iterativa
(define (product f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (f a)))
    )
  )
  (iter a 1)
)

(define (factorial-new x)
  (product identity 1 1+ x)
)

(define (pi-series n)
  (define (f-aux x)
    (if (even? x)
        (/ (+ x 2.0) (+ x 3.0))
        (/ (+ x 3.0) (+ x 2.0))
    )
  )
  (product f-aux 0.0 1+ n)
)