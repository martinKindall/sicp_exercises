; The following code is not mine, 
; it can be found at http://community.schemewiki.org/?sicp-ex-3.19
; and it is based on Robert Floyd's algorithm

(define (safe-cdr l)
	(if (pair? l)
	    (cdr l)
	    '()
	)
)

(define (contains-cycle? lst)
	(define (iter a b)
		(cond ((not (pair? a)) false)
		      ((not (pair? b)) false)
		      ((eq? a b) true)
		      ((eq? a (safe-cdr b)) true)
		      (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))
		)
	)
	(iter (safe-cdr lst) (safe-cdr (safe-cdr lst)))
)

(define (debug x)
  (newline)
  (display "----- ")
  (display x)
  (display " -----")
  (newline)
)

(define cycle '(a b c))
(define a-list '(a b c))
(set-cdr! (cddr cycle) cycle)

(debug (contains-cycle? cycle))
(debug (contains-cycle? a-list))