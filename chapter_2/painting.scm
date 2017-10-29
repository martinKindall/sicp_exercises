#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split proc1 proc2)
    (lambda (painter n) 
        (if (= n 0)
            painter
            (let ((smaller ((split proc1 proc2) painter (- n 1))))
                (proc1 painter (proc2 smaller smaller))
            )
        )
    )
)

(define right-split
    (split beside below)
)

(define up-split
    (split below beside)
)

(define (corner-split painter n)
  	(if (= n 0)
  	    painter
  	    (let ((up (up-split painter (- n 1)))
  	    	   (right (right-split painter (- n 1)))
  	    	 )
  	    	(let ((top-left (beside up up))
  	    		   (bottom-right (below right right))
  	    		   (corner (corner-split painter (- n 1)))
  	    	     )
  	    		(beside (below painter top-left) (below bottom-right corner))
  	    	)
  	    )
  	)
)

(define (square-of-four tl tr bl br)
    (lambda (painter) 
        (let ((top (beside (tl painter) (tr painter)))
               (bot (beside (bl painter) (br painter)))
             )
            (below bot top)
        )
    )
)

(define (square-limit painter n)
    ((square-of-four flip-horiz identity rotate180 flip-vert) (corner-split painter n))
)
