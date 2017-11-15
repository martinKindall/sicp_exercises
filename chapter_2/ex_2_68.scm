(define nil '())

(define (make-leaf symbol weight)
  	(list 'leaf symbol weight)
)

(define (leaf? object)
  	(eq? (car object) 'leaf)
)

(define (symbol-leaf leaf)
  	(cadr leaf)
)

(define (weigth-leaf leaf)
  	(caddr leaf)
)

; ---------- abstraction barrier ----------

(define (left-branch tree)
  	(car tree)
)

(define (right-branch tree)
  	(cadr tree)
)

(define (symbols tree)
  	(if (leaf? tree)
  	    (list (symbol-leaf tree))
  	    (caddr tree)
  	)
)

(define (weight tree)
  	(if (leaf? tree)
  	    (weigth-leaf tree)
  	    (cadddr tree)
  	)
)

(define (make-code-tree left right)
  	(list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right)))
)

; ---------- abstraction barrier ----------
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))
    )
)

(define (decode bits tree)
  	(define (choose-branch bit branch)
  	  	(cond ((= bit 0) (left-branch branch))
  	  	      ((= bit 1) (right-branch branch))
  	  	      (else (error "bad bit!: " bit))
  	  	)
  	)
  	
  	(define (decode-1 bits current-branch)
  	  	(if (null? bits)
  	  	    nil
  	  	    (let ((next-branch (choose-branch (car bits) current-branch)))
  	  	    	(if (leaf? next-branch)
  	  	    	    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
  	  	    	    (decode-1 (cdr bits) next-branch)
  	  	    	)
  	  	    )
  	  	)
  	)
  	(decode-1 bits tree)
)

(define sample-tree
	(make-code-tree 
		(make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree
				(make-leaf 'D 1)
				(make-leaf 'C 1)
			)
		)
	)
)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


(define (encode-symbol symbol tree)
    (define (iter current result)
        (let ((left (left-branch current)) (right (right-branch current)))
            (cond ((and (leaf? left) (eq? (symbol-leaf left) symbol)) (append result '(0)))
                  ((and (leaf? right) (eq? (symbol-leaf right) symbol)) (append result '(1)))
                  ((element-of-set? symbol (symbols left)) (iter left (append result '(0))))
                  ((element-of-set? symbol (symbols right)) (iter right (append result '(1))))
            )
        )
    )

    (if (not (element-of-set? symbol (symbols tree))) 
        (error "Symbol not found on tree: " symbol)
        (iter tree nil)
    )
)


(define (encode message tree)
    (if (null? message)
        nil
        (append (encode-symbol (car message) tree) (encode (cdr message) tree))
    )
)

(newline)
(display (encode '(a d a b b c a) sample-tree))
(display (encode '(a d a b b c f) sample-tree))   ; triggers error