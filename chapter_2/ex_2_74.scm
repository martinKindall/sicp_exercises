; ---- a) -----

(define (get-record name set)
    ((get 'get-record (type-tag set)) name (contents set))
)

; the division's files should be arranged as a binary tree, ordered
; by the keys (names).

; ---- b) -----

(define (get-salary record)
  	((get 'get-salary (type-tag record)) (contents record))
)

; the records could be structured as a list like the following:
; '(john (salary 1000) (address olkahoma123))

; ---- c) -----

; this procedure assumes that if get-record doesn't find a record on a division, it outputs false

(define (find-employee-record name divisions)
  	(filter 
  		(lambda (item) (not (eq? item false)))
  		(map 
  			(lambda (division) (get-record name division))
  			divisions
  		)
  	)
)