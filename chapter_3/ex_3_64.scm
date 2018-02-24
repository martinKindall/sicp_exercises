(define (stream-limit stream tolerance)
	(let ((first (stream-car stream)) (second (stream-car (stream-cdr stream))))
		(if (< (abs (- first second)) tolerance)
		    second
		    (stream-limit (stream-cdr stream) tolerance))))

(define (average x y)
  	(/ (+ x y) 2))

(define (sqrt-improve guess x)
  	(average guess (/ x guess)))

(define (sqrt-stream x)
  	(define guesses
  		(cons-stream 
  			1.0
  			(stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  	guesses)

(define (sqrt x tolerance)
	(stream-limit (sqrt-stream x) tolerance))