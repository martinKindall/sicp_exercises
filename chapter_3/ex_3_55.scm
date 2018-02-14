(define (add-streams s1 s2)
  	(stream-map + s1 s2))

(define (integers-starting-from n)
  	(cons-stream n (integers-starting-from (1+ n))))

(define integers (integers-starting-from 0))

(define (partial-sums stream)
	(cons-stream (stream-car stream) (add-streams (stream-cdr stream) (partial-sums stream))))

(define partial-integers (partial-sums integers))