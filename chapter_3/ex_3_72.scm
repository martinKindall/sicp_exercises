(define (debug x)
    (newline)
    (display "----- ")
    (display x)
    (display " -----")
    (newline)
)

(define (cube number)
    (* number (square number)))

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

(define (print-pair special-pair weight)
    (debug special-pair)
    (display (weight (car special-pair)))
    (newline))

(define (print-cicle i n stream weight)
	(if (> i n)
	    'done
	    (begin
	    	(print-pair (stream-car stream) weight)
	    	(print-cicle (1+ i) n (stream-cdr stream) weight))))

; here begins the solution

(define (special-weight pair)
    (+ (square (car pair)) (square (cadr pair))))

(define special-pairs (weighted-pairs integers integers special-weight))

(define filtered-pairs (filter-n-consecutive-weights special-pairs special-weight 3))

(print-cicle 0 10 filtered-pairs special-weight)

; thanks to the abstraction, now calculating ramanujan takes fewer lines of code

(define (ramanujan-weight pair)
    (+ (cube (car pair)) (cube (cadr pair))))

(define ramanujan-raw (weighted-pairs integers integers ramanujan-weight))

(define ramanujan-pairs (filter-n-consecutive-weights ramanujan-raw ramanujan-weight 2))

(print-cicle 0 5 ramanujan-pairs ramanujan-weight)