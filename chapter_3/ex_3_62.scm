(define (mul-streams s1 s2)
  	(stream-map * s1 s2))

(define (div-streams s1 s2)
  	(stream-map / s1 s2))

(define ones (cons-stream 1 ones))

(define (integers-starting-from n)
  	(cons-stream n (integers-starting-from (1+ n))))

(define integers (integers-starting-from 1))

(define fractions (div-streams ones integers))   ; 1, 1/2, 1/3, 1/4, ...

(define (integrate-series stream)
  	(mul-streams stream fractions))

(define cosine-series (cons-stream 1 (stream-map (lambda (elem) (* elem -1)) (integrate-series sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define (add-streams s1 s2)
  	(stream-map + s1 s2))

(define (scale-stream stream factor)
    (stream-map (lambda (element) (* element factor)) stream))

(define (series-ref serie n)
  	(stream-ref serie n))

(define (mul-series s1 s2)
  	(cons-stream 
  		(* (stream-car s1) (stream-car s2))
  		(add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (mul-series (stream-cdr s1) s2))))

(define (add-series s1 s2)
  	(add-streams s1 s2))

(define (invert-unit-series series)
  	(cons-stream 1 (scale-stream (mul-series (stream-cdr series) (invert-unit-series series)) -1)))

; ---- begins ex 3.62

(define (div-series series1 series2)   ; based on meteorgan solution on community.schemewiki.org
	(let ((c (stream-car series2)))
		(if (= c 0)
		    (error "Denominator with constant term 0")
		    (mul-series series1 (scale-stream (invert-unit-series (scale-stream series2 (/ 1 c))) (/ 1 c))))))

; abstraction barrier, now we refer to series only

(define tangent-series (div-series sine-series cosine-series))

(define (eval-series x serie n)
	(define (iter current accum)
	  	(cond ((> current n) accum)
	  	      (else (iter (1+ current) (+ accum (* (series-ref serie current) (expt x current)))))))
	(iter 0 0))

(define pi 3.14159)
(define num_of_terms 50)

(newline)
(display (eval-series (/ pi 6) tangent-series num_of_terms))   ; close to 1/sqrt(3)
(newline)
(display (eval-series (/ pi 4) tangent-series num_of_terms))   ; close to 1
(newline)
(display (eval-series (/ pi 3) tangent-series num_of_terms))   ; close to sqrt(3)
(newline)
(display (eval-series pi tangent-series num_of_terms))   ; 955654507394710.7 (huge number, but should be 0)
(newline)

(display (eval-series pi sine-series num_of_terms))   ; close to 0
(newline)
(display (eval-series pi cosine-series num_of_terms))   ; close to -1
(newline)