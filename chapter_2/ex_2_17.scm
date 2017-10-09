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
	(if (or (= (length items) 0) (< n 0))
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

;; solution 1 (maybe not efficient), it traverses the list 2 times, one with length and another time with list-ref
(define (last-pair lista)
    (list (list-ref lista (- (length-iter lista) 1)))
)

;; solution 2 recursive
(define (last-pair-v2 lista)
    (if (null? (cdr lista))
        (list (car lista))
        (last-pair-v2 (cdr lista))
    )
)