; queue pointers implementation

(define nil '())

; constructor

(define (make-queue)
  	(make-queue-from-item nil)
)

(define (make-queue-from-item item)
	(cons item nil)
)

; selectors

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

; mutators

(define (set-front-ptr! queue item)
	(set-car! queue item)
)
(define (set-rear-ptr! queue item)
  	(set-cdr! queue item)
)

; ------ abstraction barrier ------ a queue could be a pair or a list of length 2

; selector

(define (front-queue queue)
  	(if (empty-queue? queue)
	    (error "FRONT called with an empty queue" queue)
	    (car (front-ptr queue))
  	)	
)

; predicate

(define (empty-queue? queue)
  	(null? (front-ptr queue))
)

; mutators

(define (insert-queue! queue item)
  	(let ((new-pair (make-queue-from-item item)))
  		(cond ((empty-queue? queue) 
  				(set-front-ptr! queue new-pair)
  				(set-rear-ptr! queue new-pair)
  				(print-queue queue))
  		      (else 
  		      	(set-cdr! (rear-ptr queue) new-pair)
  				(set-rear-ptr! queue new-pair)
  				(print-queue queue)
  		      )
  		)
  	)
)

(define (delete-queue! queue)
  	(cond ((empty-queue? queue) (error "DELETE! called with an empty queue" (print-queue queue)))
  	      (else 
  	      	(set-front-ptr! queue (cdr (front-ptr queue)))
  	      	(print-queue queue)
  	      )
  	)
)

; procedures

(define (print-queue queue)
  	(front-ptr queue)
)