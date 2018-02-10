(define (make-mutex)
	(let ((cell (list false)))
		(define (the-mutex m)
			(cond ((eq? m 'acquire)
				   (if (test-and-set! cell)
					    (the-mutex 'acquire)))
			      ((eq? m 'release) (clear! cell))))
		the-mutex))

(define (clear! cell)
  	(set-car! cell false))

(define (test-and-set! cell)
  	(if (car cell)
  	    true
  	    (begin 
  	    	(set-car! cell true)
  	    	false
  	    )))

(define (make-semaphore n)
	(let ((cells (make-list n '(false))))
		(define (the-semaphore m)
			(cond ((eq? m 'acquire) 
				   (if (test-and-set-many! cells)
				       (the-semaphore 'acquire)))
			      ((eq? m 'release) (clear-many! cells))))
	the-semaphore))

(define (test-and-set-many! cells)
  	(cond ((null? cells) true)
  	      ((test-and-set! (car cells)) (test-and-set-many! (cdr cells)))
  	      (else false)))

(define (clear-many! cells)
  	(cond ((null? cells) (error "Releasing an empty mutex"))
  	      ((car cells) (clear! (car cells)))
  	      (else (clear-many! (cdr cells)))))