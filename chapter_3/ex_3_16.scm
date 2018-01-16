(define (count-pairs x)
  	(if (not (pair? x))
  		0
  		(+ (count-pairs (car x))
  			(count-pairs (cdr x))
  			1
  		)
  	)
)

; --------- case return 3 ---------
; list of 3 pairs
(define z1 '(a b c))

(count-pairs z1)
; 3


; --------- case not return at all ---------
(set-cdr! (cddr z1) z1)

;(count-pairs z1)
; Aborting!: maximum recursion depth exceeded


; --------- case return 4 ---------
(define y '(b))
(define x (cons 'a y))


; z2 is a list of three pairs
(define z2 (cons x y))

(count-pairs z2)
; 4

; --------- case return 7 ---------
(define w (cons y y))
(define z3 (cons w w))

(count-pairs z3)
; 7