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
    (without-interrupts
        (lambda () 
          	(if (car cell)
          	    true
          	    (begin 
          	    	(set-car! cell true)
          	    	false
          	    )))))

; implementation b, inefficient compared to the next solution, because of the loops on the cells list)

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
    ; here without-interrupts is necessary in order to not clear the same cell by two or more concurrent processes
    (without-interrupts
        (lambda () 
          	(cond ((null? cells) (error "Releasing an empty mutex"))
          	      ((car cells) (clear! (car cells)))
          	      (else (clear-many! (cdr cells))))))


; implementation b-2 (from leafac, schemewiki answers))

(define (make-semaphore maximum-clients) 
   (let ((access-mutex (list false)) 
         (clients 0)) 
     (define (the-semaphore message) 
       (cond ((eq? message 'acquire) 
              (if (test-and-set! access-mutex) 
                  (the-semaphore 'acquire)) 
              (cond ((> clients maximum-clients) 
                     (clear! access-mutex) 
                     (the-semaphore 'acquire)) 
                    (else 
                     (set! clients (+ clients 1)) 
                     (clear! access-mutex)))) 
             ((eq? message 'release) 
              (if (test-and-set! access-mutex) 
                  (the-semaphore 'release)) 
              (set! clients (- clients 1)) 
              (clear! access-mutex)))) 
     the-semaphore))