(define (show x)
	(newline)
  	(display x)
	(newline)
  	x)

(define (stream-enumerate-interval low high)
	(if (> low high)
	    the-empty-stream
	    (cons-stream
	    	low
	    	(stream-enumerate-interval (1+ low) high))))

(define x 
	(stream-map 
		show
		(stream-enumerate-interval 0 10)))

; until here, only 0 should be displayed

(stream-ref x 5)

; this displays 1 to 5

(stream-ref x 7)

; this displays 6 and 7

; each element of the stream x interval gets printed only once, the first time they are 'consumed', 
; because of the delayed map, and hence delayed calls to show.
; Then, when an element of the interval is called again, the 'delay' just calls the result stored thanks to memoization