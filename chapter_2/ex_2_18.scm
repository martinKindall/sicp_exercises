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

(define (last-pair lista)
    (list (list-ref lista (- (length-iter lista) 1)))
)


(define (append-iter list1 list2)
    (define (iter counter newList)
        (if (< counter 0)
            newList
            (iter (- counter 1) (cons (list-ref list1 counter) newList))
        )
    )
    (iter (- (length list1) 1) list2)
)

;; solution 1
(define (reverse lista)
    (let ((len (length lista)))
        (define (iter counter newList)
            (if (< counter 0)
                newList
                (iter (- counter 1) (append-iter newList (list (list-ref lista counter))))
            )
        )
        (iter (- len 1) ())
    )
)

;; solution 2

;; this procedure returns a new list, based on a list, a base-case (which is another another list) and a procedure 'f'
(define (iter-over-list lista base-case f)
    (define (iter counter newList)
        (if (< counter 0)
            newList
            (iter (-1+ counter) (f counter lista newList))
        )
    )
    (iter (- (length lista) 1) base-case)
)

(define (append-iter-v2 list1 list2)
    (iter-over-list list1 list2 (lambda (counter oldList newList) (cons (list-ref oldList counter) newList)))
)

(define (reverse-v2 lista)
    (iter-over-list lista () (lambda (counter oldList newList) (append-iter-v2 newList (list (list-ref oldList counter)))))
)

;; solution 3 recursive process

(define (reverse-v3 lista)
    (if (null? lista)
        lista
        (append-iter (reverse-v3 (cdr lista)) (list (car lista)))
    )
)

;; solution 4 iterative process

(define (reverse-v4 lista)
    (define (iter oldList newList)
        (if (null? oldList)
            newList
            (iter (cdr oldList) (append-iter (list (car oldList)) newList))
        )
    )
    (iter lista ())
)