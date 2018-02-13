(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)

(define (stream-enumerate-interval low high)
	(if (> low high)
	    the-empty-stream
	    (cons-stream
	    	low
	    	(stream-enumerate-interval (1+ low) high))))

(define (display-stream stream)
	(stream-for-each (lambda (x) (display " ")(display x)(display " ")) stream))

(define seq
	(stream-map accum
				(stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
	(stream-filter (lambda (x) (= (remainder x 5) 0))
				   seq))

(display(stream-ref y 1))				   
(newline)
(display-stream z)
(newline)
(display-stream seq)