(define (debug x)
    (newline)
    (display "----- ")
    (display x)
    (display " -----")
    (newline)
)

(define (cube number)
    (* number (square number)))

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

(define (ramanujan-weight pair)
    (+ (cube (car pair)) (cube (cadr pair))))

(define ramanujan-raw (weighted-pairs integers integers ramanujan-weight))

(define (filter-2-consecutive-weights s weight) 
    (let ((first (stream-car s)) (second (stream-car (stream-cdr s))))
        (if (= (weight first) (weight second))
            (cons-stream 
                (list first second)
                (filter-2-consecutive-weights (stream-cdr (stream-cdr s)) weight))
            (filter-2-consecutive-weights (stream-cdr s) weight))))

(define ramanujan-pairs (filter-2-consecutive-weights ramanujan-raw ramanujan-weight))

(define (print-ramanujan ramanujan-pair)
    (debug ramanujan-pair)
    (display (+ (cube (car (car ramanujan-pair))) (cube (cadr (car ramanujan-pair)))))
    (newline))

(print-ramanujan (stream-ref ramanujan-pairs 0))
(print-ramanujan (stream-ref ramanujan-pairs 1))
(print-ramanujan (stream-ref ramanujan-pairs 2))
(print-ramanujan (stream-ref ramanujan-pairs 3))
(print-ramanujan (stream-ref ramanujan-pairs 4))
(print-ramanujan (stream-ref ramanujan-pairs 5))
