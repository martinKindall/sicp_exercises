(define (ripple-carry-adder a-list b-list s-list c)
  	(let ((carry-list (append (map make-wire (cdr a-list)) (list c))))
  		(define (iter current-a-list current-b-list current-s-list current-carry-list former-carry)
  		  	(cond ((null? current-a-list) 'ok)
  		  	      (else 
  		  	      	(full-adder (car current-a-list) (car current-b-list) former-carry (car current-s-list) (car current-carry-list))
  		  	      	(iter (cdr current-a-list) (cdr current-b-list) (cdr current-s-list) (cdr current-carry-list) (car current-carry-list))
  		  	      )
  		  	)
  		)
  		(iter a-list b-list s-list carry-list (ground-wire))
  	)
)

(define (full-adder a b c-in sum c-out)
  	(let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
  		(half-adder b c-in s c1)
  		(half-adder a s sum c2)
  		(or-gate c1 c2 c-out)
  		'ok
  	)
)

(define (ground-wire)
  	; wire that returns 0 always
)