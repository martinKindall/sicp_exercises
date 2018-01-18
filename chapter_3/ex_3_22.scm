; queue pointers implementation

(define nil '())

; constructor

(define (make-queue)
    (let ((front-ptr nil) (rear-ptr nil))

        (define (set-front-ptr! new-pair)
            (set! front-ptr new-pair)
        )

        (define (set-rear-ptr! new-pair)
            (set! rear-ptr new-pair)
        )

        (define (dispatch m)
            (cond ((eq? m 'front-ptr) front-ptr)
                  ((eq? m 'rear-ptr) rear-ptr)
                  ((eq? m 'set-front-ptr!) set-front-ptr!)
                  ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            )
        )
        dispatch
    )
)

; selectors

(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))

; mutators

(define (set-front-ptr! queue item)
	((queue 'set-front-ptr!) item)
)
(define (set-rear-ptr! queue item)
  	((queue 'set-rear-ptr!) item)
)

; ------ abstraction barrier ------ a queue could be a pair or a list of length 2 or an object, like this example

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
  	(let ((new-pair (list item)))
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