(define (debug x)
    (newline)
    (display "----- ")
    (display x)
    (display " -----")
    (newline)
)

(define (merge-weighted s1 s2 weight)
  	(cond ((stream-null? s1) s2)
  	      ((stream-null? s2) s1)
  	  	  (else
  	  	  	(let ((s1car (stream-car s1)) (s2car (stream-car s2)))
  	  	  		(cond ((< (weight s1car) (weight s2car)) 
  	  	  		    	(cons-stream
  	  	  		    		s1car
  	  	  		    		(merge-weighted (stream-cdr s1) s2 weight)))
  	  	  		      ((> (weight s1car) (weight s2car)) 
  	  	  		      	(cons-stream 
  	  	  		      		s2car
  	  	  		      		(merge-weighted s1 (stream-cdr s2) weight)))
  	  	  		  	  (else
  	  	  		  	  	(cons-stream
  	  	  		  	  		s1car
                            (cons-stream
                               s2car
  	  	  		  	  		   (merge-weighted (stream-cdr s1)
  	  	  		  	  			   (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
    (cons-stream 
        (list (stream-car s) (stream-car t))
        (merge-weighted
            (stream-map (lambda (x) (list (stream-car s) x))
                                    (stream-cdr t))
            (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
            weight)))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (1+ n))))

(define integers (integers-starting-from 1))

(define integer-pairs-a (weighted-pairs integers integers (lambda (pair) (apply + pair))))