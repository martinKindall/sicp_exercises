(define (new-cat name)
  	(define (cat-object action)
    	(cond ((eq? action 'talk) 
    			(newline)
    			(display (string-append name " says: meooow"))
    			(newline))
    	      (else (error "undefined method"))
    	)
	)
	cat-object
)

(define (new-person name age)
  	(define (person-object action)
    	(cond ((eq? action 'talk) 
    			(newline)
    			(display (string-append "Hello, my name is " name " and I am " age " years old."))
    			(newline))
    	      (else (error "undefined method"))
    	)
	)
	person-object
)

(define cat (new-cat "Tom"))
(define human (new-person "John" "28"))

; polymorphism call

(for-each (lambda (object) (object 'talk)) (list cat human))