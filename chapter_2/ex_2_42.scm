; ------------------- eight queens puzzle --------------------

; ------------------- general methods --------------------

(define (enumerate a b)
    (define (iter current result)
        (if (> current b)
            result
            (iter (1+ current) (append result (list current)))
        )
    )
    (iter a ())
)

(define (accumulate op initial sequence)
  	(cond ((null? sequence) initial)
  	      (else (op (car sequence) (accumulate op initial (cdr sequence))))
  	)
)

(define (map proc sequence)
  	(accumulate 
  		(lambda (x y)
  			(cons (proc x) y)
	  	) 
    	'()
    	sequence	
	)
)

(define (flatmap proc seq)
  	(accumulate append '() (map proc seq))
)

; ------------------- general methods --------------------

; ------------------- eight queens puzzle --------------------

(define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (make-board-position k new-row))
)

(define (safe? k positions)
    (let ((current (get-position k positions)))
        (define (safe-iter positions)
            (cond ((null? positions) true)
                  ((or (same-row? (pop-position positions) current)
                        (in-diagonal? (pop-position positions) current)
                    )
                     false)
                  (else (safe-iter (cdr positions)))
            )
        )
        (safe-iter (remove-position k positions))
    )
)

(define (remove-position k positions)
    (filter (lambda (pair) (not (= k (get-col pair)))) positions)
)

(define (same-position? pos1 pos2)
    (and (= (get-col pos1) (get-col pos2)) (= (get-row pos1) (get-row pos2)))
)

(define (same-row? pos1 pos2)
    (= (get-row pos1) (get-row pos2))
)

; wrong procedure
(define (in-diagonal?-wrong pos1 current)
    (if (or (= 0 (get-row pos1)) 
            (> (get-row pos1) (1+ (max (get-col current) (get-row current))))
            (> (get-col pos1) (get-col current))
            (and (same-row? pos1 current) (not (same-position? pos1 current)))
        )
        false
        (if (same-position? pos1 current)
            true
            (if (< (get-row pos1) (get-row current)) 
                (in-diagonal? (make-position (1+ (get-col pos1)) (1+ (get-row pos1))) current)
                (in-diagonal? (make-position (1+ (get-col pos1)) (-1+ (get-row pos1))) current) 
            ) 
        )
    )
)

(define (in-diagonal? pos1 pos2)
    (= (abs (- (get-row pos1) (get-row pos2))) (abs (- (get-col pos1) (get-col pos2))))
)

(define (get-position k positions)
    (pop-position (filter (lambda (position) (= k (get-col position))) positions))
)

; ------ representation of sets of board positions ---------

(define empty-board '())

(define (make-board-position col row)
    (list (make-position col row))
)

(define (make-position col row)
    (list col row)
)

(define (get-col position)
    (car position)
)

(define (get-row position)
    (cadr position)
)

(define (pop-position positions)
    (car positions)
)

; ------ representation of sets of board positions ---------

; ------------ main ------------

(define (queens board-size)
  	(define (queen-cols k)
  	  	(if (= k 0)
  	  	    (list empty-board)
  	  	    (filter
  	  	    	(lambda (positions) (safe? k positions))
  	  	    	(flatmap 
  	  	    		(lambda (rest-of-queens) 
  	  	    			(map 
  	  	    				(lambda (new-row) 
  	  	    					(adjoin-position new-row k rest-of-queens)
  	  	    				)
  	  	    				(enumerate 1 board-size)
  	  	    			)
  	  	    		)
  	  	    		(queen-cols (-1+ k))
  	  	    	)
  	  	    )
  	  	)
  	)
  	(queen-cols board-size)
)

; ------------ main ------------