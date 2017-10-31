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

(define (flip-horiz-custom painter)
  	(transform-painter painter (make-vect 1 0) (make-vect 0 0) (make-vect 1 1))
)

(paint (beside einstein (flip-horiz-custom einstein)))

(define (rotate180-custom painter)
  	(transform-painter painter (make-vect 1 1) (make-vect 0 1) (make-vect 1 0))
)

(paint (rotate180-custom einstein))

(define (rotate270-custom painter)
  	(transform-painter painter (make-vect 0 1) (make-vect 0 0) (make-vect 1 1))
)

(paint (rotate270-custom einstein))
