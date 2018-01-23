(define (make-table)
	
	(define (assoc key records)
	  	(cond ((null? records) false)
	  	      ((equal? key (caar records)) (car records))
	  	      (else (assoc key (cdr records)))
	  	)
	)

	(let ((local-table (list '*table*)))
		; private methods

		(define (lookup . list-keys)
			(define (iter current-keys current-table)
				(let ((subtable (assoc (car current-keys) (cdr current-table))))
					;(debug subtable)
					(if subtable
					    (if (null? (cdr current-keys))
					        (cdr subtable)
					        (iter (cdr current-keys) subtable)
					    )
					    false
					)
				)
			)
			(iter list-keys local-table)
		)

		(define (insert! value . list-keys)

		  	(define (form-list list-keys value)
		  		(let ((first-key (car list-keys)))
		  			(cond ((null? (cdr list-keys)) (cons first-key value))
		  			      (else (list first-key (form-list (cdr list-keys) value)))
		  			)
		  		)
		  	)

		  	(define (iter current-keys current-table)
				(let ((subtable (assoc (car current-keys) (cdr current-table))))
					;(debug subtable)
					(if subtable
					    (if (null? (cdr current-keys))
					        (set-cdr! subtable value)
						    (iter (cdr current-keys) subtable)
					    )
					    (if (null? (cdr current-keys))
					        (set-cdr! current-table (cons (cons (car current-keys) value) (cdr current-table)))
						    (set-cdr! current-table (cons (form-list current-keys value) (cdr current-table)))
					    )
					)
				)
			)
			(iter list-keys local-table)

		  	'ok
		)

		; messages

		(define (dispatch m)
		  	(cond ((eq? m 'lookup-proc) lookup)
		  	      ((eq? m 'insert-proc!) insert!)
		  	      ((eq? m 'view) local-table)
		  	      (else (error "Unknown operation: TABLE" m))
		  	)
		)

		dispatch
	)
)

(define (debug x)
	(newline)
	(display "----- ")
	(display x)
	(display " -----")
	(newline)
)

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define view-table (operation-table 'view))

(put 'tamarugo 'arboles 'chile)
(put 'mango 'arboles 'peru)
(put 'banano 'arboles 'colombia)

(put 'sqrt 'math 'functions 'square-root)
(put 50 'math 'operations '-)
(put 43 'math 'operations '+)

view-table