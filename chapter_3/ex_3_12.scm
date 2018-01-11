(define x '(a b))
(define y '(c d))
(define z (append x y))

(newline)
(display (cdr x))
(newline)

(define w (append! x y))

(display (cdr x))
(newline)