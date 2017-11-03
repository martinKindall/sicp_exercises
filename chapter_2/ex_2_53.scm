(define res1 (list 'a 'b 'c))
(define res2 (list (list 'george)))
(define res3 (cdr '((x1 x2) (y1 y2))))
(define res4 (cadr '((x1 x2) (y1 y2))))
(define res5 (pair? (car '(a short list))))
(define res6 (memq 'red '((red shoes) (blue socks))))
(define res7 (memq 'red '(red shoes blue socks)))

(define results (list res1 res2 res3 res4 res5 res6 res7))

(for-each (lambda (item) (newline)(display item)) results)