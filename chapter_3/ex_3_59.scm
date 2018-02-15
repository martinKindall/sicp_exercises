; part a)

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

; part b)

(define cosine-series (cons-stream 1 (stream-map (lambda (elem) (* elem -1)) (integrate-series sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))