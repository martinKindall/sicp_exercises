(define (length items)
  	(if (null? items)
  	    0
  	    (+ 1 (length (cdr items)))
  	)
)

(define (length-iter items)
  	(define (iter counter items)
  		(if (null? items)
  		    counter
  		    (iter (1+ counter) (cdr items))
  		)
  	)
  	(iter 0 items)
)

(define (list-ref items n)
	(if (= (length items) 0)
	    ()   ;; nil or end-of-sequence
	    (if (= n 0)
	  	    (car items)
	  	    (list-ref (cdr items) (- n 1))
	  	)
	)
)

(define (append list1 list2)
  	(if (null? list1)
  	    list2
  	    (cons (car list1) (append (cdr list1) list2))
  	)
)