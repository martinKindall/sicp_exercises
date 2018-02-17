(define (invert-unit-series series)
  	(cons-stream 1 (scale-stream (mul-series (stream-cdr series) (invert-unit-series series)) -1)))