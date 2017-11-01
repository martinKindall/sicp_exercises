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

(define (corner-split-mod painter n)
  	(if (= n 0)
  	    painter
  	    (let ((up (up-split painter (- n 1)))
  	    	   (right (right-split painter (- n 1)))
  	    	 )
  	    	(let ((top-left up)
  	    		   (bottom-right right)
  	    		   (corner (corner-split-mod painter (- n 1)))
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

(define (square-limit-mod painter n)
    ((square-of-four flip-horiz identity rotate180 flip-vert) (flip-horiz (corner-split-mod painter n)))
)

; Thanks to 'Bill the Lizard' for sharing wave definition

(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))
  (make-segment   ; this is a line in the face
    (make-vect 0.4 0.7)
    (make-vect 0.6 0.7)
  )
))

(define wave (segments->painter wave-segments))

(paint (corner-split-mod wave 3))

(paint (square-limit-mod wave 2))