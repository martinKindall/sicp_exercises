(define nil '())

(define (make-leaf symbol weight)
  	(list 'leaf symbol weight)
)

(define (leaf? object)
  	(eq? (car object) 'leaf)
)

(define (symbol-leaf leaf)
  	(cadr leaf)
)

(define (weigth-leaf leaf)
  	(caddr leaf)
)

; ---------- abstraction barrier ----------

(define (left-branch tree)
  	(car tree)
)

(define (right-branch tree)
  	(cadr tree)
)

(define (symbols tree)
  	(if (leaf? tree)
  	    (list (symbol-leaf tree))
  	    (caddr tree)
  	)
)

(define (weight tree)
  	(if (leaf? tree)
  	    (weigth-leaf tree)
  	    (cadddr tree)
  	)
)

(define (make-code-tree left right)
  	(list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right)))
)

; ---------- abstraction barrier ----------

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set) (adjoin-set x (cdr set))))
    )
)

(define (make-leaf-set pairs)
    (if (null? pairs)
        nil
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs)))
        )
    )
)

(define (successive-merge nodes)
    (let ((len (length nodes)))
        (if (= len 1) 
            nodes
            (let ((node1 (car nodes)) (node2 (cadr nodes)))
                (let ((new-tree (make-code-tree node1 node2)))
                    (successive-merge (adjoin-set new-tree (cddr nodes)))
                )
            )
        )
    )
)

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
)

(define example-tree
    (car (generate-huffman-tree '((a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1))))
)

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))
    )
)

(define (encode-symbol symbol tree)
    (define (iter current result)
        (let ((left (left-branch current)) (right (right-branch current)))
            (cond ((and (leaf? left) (eq? (symbol-leaf left) symbol)) (append result '(0)))
                  ((and (leaf? right) (eq? (symbol-leaf right) symbol)) (append result '(1)))
                  ((element-of-set? symbol (symbols left)) (iter left (append result '(0))))
                  ((element-of-set? symbol (symbols right)) (iter right (append result '(1))))
            )
        )
    )

    (if (not (element-of-set? symbol (symbols tree))) 
        (error "Symbol not found on tree: " symbol)
        (iter tree nil)
    )
)


(define (encode message tree)
    (if (null? message)
        nil
        (append (encode-symbol (car message) tree) (encode (cdr message) tree))
    )
)

(define encoded-msg (encode '(B A C A D A E A F A B B A A A G A H) example-tree))


; note that the string of bits that comes out from the previous
; encode doesn't match the one shown in the book, but that's ok,
; because our example-tree has its branches arranged in a 
; different way, because the choice of the order in which
; the nodes are merged in 'successive-merge' is arbitrary 

(define (decode bits tree)
    (define (choose-branch bit branch)
        (cond ((= bit 0) (left-branch branch))
              ((= bit 1) (right-branch branch))
              (else (error "bad bit!: " bit))
        )
    )
    
    (define (decode-1 bits current-branch)
        (if (null? bits)
            nil
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)
                )
            )
        )
    )
    (decode-1 bits tree)
)

(decode encoded-msg example-tree)

; (b a c a d a e a f a b b a a a g a h)