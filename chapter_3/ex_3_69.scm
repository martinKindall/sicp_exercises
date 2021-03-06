(define (debug x)
	(newline)
	(display "----- ")
	(display x)
	(display " -----")
	(newline)
)

(define (integers-starting-from n)
  	(cons-stream n (integers-starting-from (1+ n))))

(define integers (integers-starting-from 1))

(define (interleave s1 s2)
  	(if (stream-null? s1)
  	    s2
  	    (cons-stream 
  	    	(stream-car s1)
  	    	(interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  	(cons-stream 
  		(list (stream-car s) (stream-car t))
  		(interleave
			(stream-map (lambda (x) (list (stream-car s) x))
									(stream-cdr t))
  			(pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
	(define pairs_t_u (pairs t u))
	(define (triples-aux current_s current_pairs_t_u) 
		(cons-stream
			(append (list (stream-car current_s)) (stream-car current_pairs_t_u))
			(interleave
				(stream-map (lambda (x) (append (list (stream-car current_s)) x)) (stream-cdr current_pairs_t_u))
				(triples-aux (stream-cdr current_s) (stream-cdr current_pairs_t_u)))))
	(triples-aux s pairs_t_u))

(define int_triples (triples integers integers integers))

(define pythagorean_triples (stream-filter 
	(lambda (x) 
		(and 
			(<= (car x) (cadr x)) 
			(= (+ (square (car x)) (square (cadr x))) (square (caddr x)))))
	int_triples))

; ----- second solution by meteorgan + adams

(define (triples-v2 s t u)
        (cons-stream (list 
		    (stream-car s)
		    (stream-car t) 
		    (stream-car u))
		(interleave
		    (stream-map (lambda (x) (cons (stream-car s) x))
		                           (stream-cdr (pairs t u)))
		    (triples-v2 (stream-cdr s)
		                 (stream-cdr t)
		                 (stream-cdr u)))))

(define (phythagorean-numbers)
        (define (square x) (* x x))
        (define numbers (triples-v2 integers integers integers))
        (stream-filter (lambda (x) 
		    (= (square (caddr x)) 
		    (+ (square (car x)) (square (cadr x)))))
		numbers))