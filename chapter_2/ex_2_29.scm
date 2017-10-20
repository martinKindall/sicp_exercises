; list implementation

;(define (make-mobile left right)
;    (list left right)
;)

;(define (make-branch length structure)
;    (list length structure)
;)

;(define (right-branch mobile)
;    (cadr mobile)
;)

;(define (branch-structure branch)
;    (cadr branch)
;)

; pair implementation

(define (make-mobile left right)
    (cons left right)
)

(define (make-branch length structure)
    (cons length structure)
)

(define (left-branch mobile)
    (car mobile)
)


(define (right-branch mobile)
    (cdr mobile)
)

(define (branch-length branch)
    (car branch)
)

(define (branch-structure branch)
  	(cdr branch)
)

;; --------------- abstraction barrier ---------------

(define (total-weight mobile)
  	(+ (branch-weigth (left-branch mobile)) (branch-weigth (right-branch mobile)))
)

(define (branch-weigth branch)
	(let ((structure (branch-structure branch)))
		(cond ((not (pair? structure)) structure)
			(else (total-weight structure))
  		)
	)
)

(define (balanced-mobile? mobile)
    (and  
        (= (torque (left-branch mobile)) (torque (right-branch mobile)))
        (balanced-branch? (left-branch mobile))    
        (balanced-branch? (right-branch mobile))    
    )
)

(define (torque branch)
    (* (branch-length branch) (branch-weigth branch))
)

(define (balanced-branch? branch)
    (let ((structure (branch-structure branch)))
        (if (pair? structure)
            (balanced-mobile? structure)
            true
        )
    )
)