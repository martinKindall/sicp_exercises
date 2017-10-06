(define (make-point x y)
  	(cons x y)
)

(define (x-point point)
  	(car point)
)

(define (y-point point)
  	(cdr point)
)

(define (print-p p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline)
)

(define (distance-p p1 p2)
    (sqrt (+ (square (- (x-point p1) (x-point p2))) (square (- (y-point p1) (y-point p2)))))
)

(define (make-segment p1 p2)
  (cons p1 p2)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

(define (print-seg seg)
    (print-p (start-segment seg))
    (print-p (end-segment seg))
)

;; first representation of rect
;(define (make-rect seg1 seg2)
;  (cons seg1 seg2)
;)

;(define (top-rect rect)
;    (car rect)
;)

;(define (bot-rect rect)
;    (cdr rect)
;)

;(define (print-rect rect)
;    (print-seg (top-rect rect))
;    (print-seg (bot-rect rect))
;)

;(define (height-rect rect)
;    (distance-p (start-segment (top-rect rect)) (start-segment (bot-rect rect)))
;)

;(define (long-rect rect)
;    (distance-p (start-segment (top-rect rect)) (end-segment (top-rect rect)))
;)

;; second representation
(define (make-rect p1 p2 p3 p4)
    (cons (cons p1 p2) (cons p3 p4))
)

(define (p1-rect rect)
    (car (car rect))
)

(define (p2-rect rect)
    (cdr (car rect))
)

(define (p3-rect rect)
    (car (cdr rect))
)

(define (p4-rect rect)
    (cdr (cdr rect))
)

(define (print-rect rect)
    (print-p (p1-rect rect))
    (print-p (p2-rect rect))
    (print-p (p3-rect rect))
    (print-p (p4-rect rect))
)

(define (height-rect rect)
    (distance-p (p1-rect rect) (p4-rect rect))
)

(define (long-rect rect)
    (distance-p (p1-rect rect) (p2-rect rect))
)


;; abstraction barrier

(define (area-rect rect)
    (* (height-rect rect) (long-rect rect))
)

(define (perim-rect rect)
    (+ (* 2 (height-rect rect)) (* 2 (long-rect rect)))
)