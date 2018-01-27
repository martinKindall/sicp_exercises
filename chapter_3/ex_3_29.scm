(define (or-gate a1 a2 output)
	(let ((b1 (make-wire)) (b2 (make-wire)) (b3 (make-wire)))
		(inverter a1 b1)
		(inverter a2 b2)
		(and-gate b1 b2 b3)
		(inverter b3 output)
	)
)

; the total time is 2*inverter-delay + 1*and-gate-delay