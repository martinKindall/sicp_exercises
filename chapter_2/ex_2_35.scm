(define (accumulate op initial sequence)
  	(cond ((null? sequence) initial)
  	      (else (op (car sequence) (accumulate op initial (cdr sequence))))
  	)
)

;(define (count-leaves tree)
;  	(accumulate ?? ?? (map ?? ??))
;)

; the following definition works but violates the signal-flow structure

(define (count-leaves-wrong tree)
  	(accumulate (lambda (x y) 
  		(if (not (pair? x))
  		    (+ 1 y)
  		    (+ (count-leaves-wrong x) y)
  		)
  	) 0 tree)
)

; another definition that obeys singal-flow, but in terms of length

(define (enumerate-tree tree)
  	(define (iter original result)
  	  	(cond ((null? original) result)
  	  	      ((pair? original) (iter (cdr original) (append result (enumerate-tree (car original)))))
  	  	      (else (list original))
  	  	)
  	)
  	(iter tree ())
)

(define (length sequence)
  	(accumulate (lambda (x y) (+ 1 y)) 0 sequence)
)

(define (count-leaves-v2 tree)
  	(length (enumerate-tree tree))
)

; another definition 

(define (count-leaves-v3 tree)
  	(accumulate + 0 (map (lambda (x) 1) (enumerate-tree tree)))
)
