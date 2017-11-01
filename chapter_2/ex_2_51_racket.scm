#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (frame-coord-map frame)
  	(lambda (vector) 
  		(vector-add
  			(frame-origin frame)
  			(vector-add
  				(vector-scale (vector-xcor vector) (frame-edge1 frame))
  				(vector-scale (vector-ycor vector) (frame-edge2 frame))
  			)
  		)
  	)
)

;------------------------------------------------

(define (transform-painter painter origin corner1 corner2)
  	(lambda (frame) 
  		(let ((m (frame-coord-map frame)))
  			(let ((new-origin (m origin)))
  				(painter (make-frame
  								new-origin
  								(vector-sub (m corner1) new-origin)
  								(vector-sub (m corner2) new-origin)
  						 )
  				)
  			)
  		)
  	)
)

(define (below-custom painter1 painter2)
  	(let ((split-point (make-vect 0 0.5)))
  		(let ((paint-bot (transform-painter painter1 (make-vect 0 0) (make-vect 1 0) split-point))
  				(paint-top (transform-painter painter2 split-point (make-vect 1 0.5) (make-vect 0 1)))
  				)
  			(lambda (frame) 
  				(paint-bot frame)
  				(paint-top frame)
  			)
  		)
  	)
)

(define (rotate180-custom painter)
  	(transform-painter painter (make-vect 1 1) (make-vect 0 1) (make-vect 1 0))
)

(define (rotate270-custom painter)
  	(transform-painter painter (make-vect 0 1) (make-vect 0 0) (make-vect 1 1))
)

(define (below-custom-2 painter1 painter2)
  	(let ((paint1_90 (rotate270-custom painter1))
  			(paint2_90 (rotate270-custom painter2))
  		  )
  		(let ((paint_1_2 (beside paint1_90 paint2_90)))
  			(lambda (frame) 
  				((rotate270-custom (rotate180-custom paint_1_2)) frame)
  			)
  		)
  	)
)

(paint (below-custom einstein einstein))
(paint (below-custom-2 einstein einstein))