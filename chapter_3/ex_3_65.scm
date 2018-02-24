(define (add-streams s1 s2)
  	(stream-map + s1 s2))

(define (partial-sums stream)
	(cons-stream (stream-car stream) (add-streams (stream-cdr stream) (partial-sums stream))))

(define (ln2-sumands n)
  	(cons-stream (/ 1.0 n)
  				 (stream-map - (ln2-sumands (1+ n)))))

(define ln2-stream
	(partial-sums (ln2-sumands 1)))

(define (euler-transform s)
  	(let ((s0 (stream-ref s 0)) (s1 (stream-ref s 1)) (s2 (stream-ref s 2)))
  		(cons-stream (- s2 (/ (square (- s2 s1)) (+ s0 (* -2 s1) s2)))
  					 (euler-transform (stream-cdr s)))))

(define ln2-stream-transformed
	(euler-transform ln2-stream))

(define (make-tableau transform s)
  	(cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  	(stream-map stream-car (make-tableau transform s)))

(define ln2-final
	(accelerated-sequence euler-transform ln2-stream))