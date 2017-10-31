#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define origin (make-vect 0 0))
(define opposite-diag (make-vect 0.95 0.95))
(define right-edge (make-vect 0.95 0))
(define left-edge (make-vect 0 0.95))

(define midpoint1 (make-vect 0 0.5))
(define midpoint2 (make-vect 1 0.5))
(define midpoint3 (make-vect 0.5 0))
(define midpoint4 (make-vect 0.5 1))

;--------- BOX ---------

(define seg1 (make-segment origin left-edge))
(define seg2 (make-segment left-edge opposite-diag))
(define seg3 (make-segment opposite-diag right-edge))
(define seg4 (make-segment right-edge origin))

(define box (segments->painter (list seg1 seg2 seg3 seg4)))

;--------- CROSS ---------

(define seg5 (make-segment origin opposite-diag))
(define seg6 (make-segment right-edge left-edge))

(define cross (segments->painter (list seg5 seg6)))

;--------- DIAMOND ---------

(define seg7 (make-segment midpoint1 midpoint3))
(define seg8 (make-segment midpoint3 midpoint2))
(define seg9 (make-segment midpoint2 midpoint4))
(define seg10 (make-segment midpoint4 midpoint1))

(define diamond (segments->painter (list seg7 seg8 seg9 seg10)))

(paint box)
(paint cross)
(paint diamond)
