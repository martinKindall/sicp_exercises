(define the-empty-stream '())

(define (stream-enumerate-interval low high)
	(if (> low high)
	    the-empty-stream
	    (cons-stream
	    	low
	    	(stream-enumerate-interval (1+ low) high))))

(define (stream-for-each proc stream)
  	(if (stream-null? stream)
  	    'done
  	    (begin 
  	    	(proc (stream-car stream))
  	    	(stream-for-each proc (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  	(if (stream-null? (car argstreams))
  	    the-empty-stream
  	    (cons-stream
  	    	(apply proc (map (lambda (stream) (stream-car stream)) argstreams))
  	    	(apply stream-map 
  	    		   (cons proc (map (lambda (stream) (stream-cdr stream)) argstreams))))))

; testing

(define stream-A (stream-enumerate-interval 10 20))
(define stream-B (stream-enumerate-interval 0 10))

(define stream-C (stream-map + stream-A stream-B))

(stream-for-each (lambda (x) (display " ")(display x)(display " ")) stream-C)