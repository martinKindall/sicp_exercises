; table module

(define table (list)) 

(define (put op type proc) 
    (set! table (append table (list (list op type proc)))) 
)

(define (get op type) 
    (define (search op type t) 
        (cond ((null? t) #f) 
            ((and (equal? (caar t) op) (equal? (cadar t) type)) 
                (caddar t) 
            ) 
            (else (search op type (cdr t)))
        ) 
    ) 
    (search op type table) 
)

(define (attach-tag type-tag contents) 
    (cons type-tag contents) 
) 

(define (type-tag datum)
    (cond ((number? datum) 'scheme-number)
          ((pair? datum) (car datum))
          (else (error "Bad tagged datum: TYPE_TAG" datum))
    )
)

(define (contents datum)
    (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (else (error "Bad tagged datum: CONTENTS " datum))
    )
)

; ------/table module------

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these tpyes: APPLY-GENERIC " (list op type-tags))
            )
        )
    )
)

(define (install-scheme-number-package)
    (put 'add '(scheme-number scheme-number) +)
    (put 'sub '(scheme-number scheme-number) -)
    (put 'mul '(scheme-number scheme-number) *)
    (put 'div '(scheme-number scheme-number) /)
)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))