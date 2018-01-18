; queue pointers implementation

(define nil '())

; constructor

(define (make-queue)
	(make-queue-pointers nil nil)
)

(define (make-queue-pointers front rear)
    (cons front rear)
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

(define (make-doubly-linked-list-item item)
    (cons (cons item nil) nil)
)

(define (next-item queue)
    (cdr (front-ptr queue))
)

; selector

(define (front-queue queue)
  	(if (empty-queue? queue)
	    (error "FRONT called with an empty queue" queue)
	    (caar (front-ptr queue))
  	)	
)

; predicate

(define (empty-queue? queue)
  	(null? (front-ptr queue))
)

; mutators

(define (insert-queue! queue item)
  	(let ((new-pair (make-doubly-linked-list-item item)))
  		(cond ((empty-queue? queue) 
  				(set-front-ptr! queue new-pair)
  				(set-rear-ptr! queue new-pair)
  				(print-queue queue))
  		      (else 
                (set-cdr! (rear-ptr queue) new-pair)
  		      	(set-cdr! (car new-pair) (rear-ptr queue))
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

(define (print-queue initial-queue) 
   (define (iter res current-queue) 
     (if (or (null? current-queue) (empty-queue? current-queue)) res 
       (iter (append res (list (front-queue current-queue))) (make-queue-pointers (next-item current-queue) (rear-ptr initial-queue))))) 
   (iter nil initial-queue))

; ------ deque implementation ------ 

; constructor

(define (make-deque) (make-queue))

; predicate

(define (empty-deque? deque)
    (empty-queue? deque)
)

; selectors

(define (front-deque deque)
    (front-queue deque)
)

(define (rear-deque deque)
    (if (empty-queue? deque)
        (error "REAR called with an empty deque" deque)
        (caar (rear-ptr deque))
    )   
)

; mutators

(define (front-insert-deque! deque item)
    (cond ((empty-deque? deque)
            (rear-insert-deque! deque item))
          (else
            (let ((new-pair (make-doubly-linked-list-item item)))
                (set-cdr! (car (front-ptr deque)) new-pair)
                (set-cdr! new-pair (front-ptr deque))
                (set-front-ptr! deque new-pair)
                (print-queue deque)
            )
          )
    )
)

(define (rear-insert-deque! deque item)
    (insert-queue! deque item)
)

(define (front-delete-deque! deque)
    (delete-queue! deque)
)

(define (rear-delete-deque! deque)
    (cond ((empty-queue? deque) (error "REAR-DELETE! called with an empty queue" (print-queue deque)))
          (else 
            (set-rear-ptr! deque (cdar (rear-ptr deque)))
            (if (null? (rear-ptr deque)) 
                (set-front-ptr! deque nil)
                (set-cdr! (rear-ptr deque) nil)
            ) 
            (print-queue deque)
          )
    )
)