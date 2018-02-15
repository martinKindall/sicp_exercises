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

; --- previous excercise, now begins 3.60

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

; abstraction barrier, now we refer to series only

(define sin2cos2-series (add-series (mul-series cosine-series cosine-series) (mul-series sine-series sine-series)))

(define (eval-series x serie n)
	(define (iter current accum)
	  	(cond ((> current n) accum)
	  	      (else (iter (1+ current) (+ accum (* (series-ref serie current) (expt x current)))))))
	(iter 0 0))

(define pi 3.14159)
(define num_of_terms 50)

(newline)
(display (eval-series pi sine-series num_of_terms))   ; close to 0
(newline)
(display (eval-series pi cosine-series num_of_terms))   ; close to -1
(newline)
(display (eval-series pi sin2cos2-series num_of_terms))   ; outputs 1
(newline)
(display (eval-series (/ pi 2) sin2cos2-series num_of_terms))   ; outputs 1
(newline)
(display (eval-series 400 sin2cos2-series num_of_terms))   ; outputs 1
(newline)