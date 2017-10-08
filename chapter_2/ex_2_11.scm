(define (make-interval a b)
  	(cons a b)
)

(define (upper-b inter)
  	(cdr inter)
)

(define (lower-b inter)
  	(car inter)
)

;; abstraction barrier

(define (spans-zero? inter)
    (and (<= (lower-b inter) 0) (>= (upper-b inter) 0))
)

(define (add-interval x y)
  	(make-interval 
  		(+ (lower-b x) (lower-b y))
  		(+ (upper-b x) (upper-b y))
  	)
)

(define (mul-interval x y)
  	(let (
  			(p1 (* (lower-b x) (lower-b y)))
  			(p2 (* (lower-b x) (upper-b y)))
  			(p3 (* (upper-b x) (lower-b y)))
  			(p4 (* (upper-b x) (upper-b y)))
  		)
  		(make-interval
  			(min p1 p2 p3 p4)
  			(max p1 p2 p3 p4)
  		)
  	)
)

(define (mul-interval-v3 x y)
  (let (
        (xlo (lower-b x))
        (xhi (upper-b x))
        (ylo (lower-b y))
        (yhi (upper-b y))
      )
    ;; 9 cases
    ;; +,+ +,+
    ;; -,- +,+
    ;; -,+ +,+
    ;; +,+ -,+
    ;; +,+ -,-
    ;; -,+ -,+
    ;; -,- -,-
    ;; -,- -,+
    ;; -,+ -,-
    (cond ((and (> xlo 0) (> xhi 0) (> ylo 0) (> yhi 0)) (make-interval
                                                            (* xlo ylo)
                                                            (* xhi yhi)
                                                          )
          )
          ((and (< xlo 0) (< xhi 0) (> ylo 0) (> yhi 0)) (make-interval
                                                            (* xlo yhi)
                                                            (* xhi ylo)
                                                          )
          )
          ((and (< xlo 0) (> xhi 0) (> ylo 0) (> yhi 0)) (make-interval
                                                            (* xlo yhi)
                                                            (* xhi yhi)
                                                          )
          )
          ((and (> xlo 0) (> xhi 0) (< ylo 0) (> yhi 0)) (make-interval
                                                            (* xhi ylo)
                                                            (* xhi yhi)
                                                          )
          )
          ((and (> xlo 0) (> xhi 0) (< ylo 0) (< yhi 0)) (make-interval
                                                            (* xhi ylo)
                                                            (* xlo yhi)
                                                          )
          )
          ((and (< xlo 0) (> xhi 0) (< ylo 0) (> yhi 0)) (make-interval
                                                            (min (* xhi ylo) (* xlo yhi))
                                                            (max (* xlo ylo) (* xhi yhi))
                                                          )
          )
          ((and (< xlo 0) (< xhi 0) (< ylo 0) (< yhi 0)) (make-interval
                                                            (* xhi yhi)
                                                            (* xlo ylo)
                                                          )
          )
          ((and (< xlo 0) (< xhi 0) (< ylo 0) (> yhi 0)) (make-interval
                                                            (* xlo yhi)
                                                            (* xlo ylo)
                                                          )
          )
          ((and (< xlo 0) (> xhi 0) (< ylo 0) (< yhi 0)) (make-interval
                                                            (* xhi ylo)
                                                            (* xlo ylo)
                                                          )
          )
    )
  )
)


(define (div-interval x y)
    (if (spans-zero? y)
        (error "denominator spans zero")
        (mul-interval
          x
          (make-interval
            (/ 1 (upper-b y))
            (/ 1 (lower-b y))
          )
        )
    )
)

(define (sub-interval x y)
    (add-interval x
      (make-interval
        (* -1 (upper-b y))
        (* -1 (lower-b y))
      )
    )
)