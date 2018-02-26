(define (debug x)
    (newline)
    (display "----- ")
    (display x)
    (display " -----")
    (newline)
)

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (1+ n))))

(define integers (integers-starting-from 1))

; filtering same weighted consecutive pairs

(define (filter-n-consecutive-weights s weight n)
	(let ((sub-list (exctract-n-items s n)))
		(if (same-elements? (map weight sub-list))
		    (cons-stream 
		    	sub-list
		    	(filter-n-consecutive-weights (skip-stream s n) weight n))
		    (filter-n-consecutive-weights (stream-cdr s) weight n))))

(define (exctract-n-items s n)
  	(if (= n 0)
  	    '()
  	    (cons (stream-car s) (exctract-n-items (stream-cdr s) (-1+ n)))))

(define (skip-stream s n)
  	(if (= n 0)
  	    s
  	    (skip-stream (stream-cdr s) (-1+ n))))

(define (same-elements? list)
  	(if (or (null? list) (= (length list) 1))
  	    true
  	    (if (not (= (car list) (cadr list)))
  	        false
  	        (same-elements? (cdr list)))))

; --/filtering same weighted consecutive pairs

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

(define (filter-3-consecutive-weights s weight)
  	(filter-n-consecutive-weights s weight 3))

(define (special-weight pair)
    (+ (square (car pair)) (square (cadr pair))))

(define special-pairs (weighted-pairs integers integers special-weight))

(define filtered-pairs (filter-3-consecutive-weights special-pairs special-weight))

(define (print-pair special-pair)
    (debug special-pair)
    (display (+ (square (car (car special-pair))) (square (cadr (car special-pair)))))
    (newline))

(define (print-cicle i n)
	(if (> i n)
	    'done
	    (begin
	    	(print-pair (stream-ref filtered-pairs i))
	    	(print-cicle (1+ i) n))))

(print-cicle 0 20)