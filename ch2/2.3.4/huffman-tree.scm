(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
	 (list left
	       right
	       (append (symbols left) (symbols right))
	       (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits node)
    (if (null? bits)
	'()
	(let ((next-node
	       (choose-branch (car bits) node)))
	       (if (leaf? next-node)
		   (cons (symbol-leaf next-node)
			 (decode-1 bits tree))
		   (decode-1 (cdr bits) next-node)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pairs))
		    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree symbols)
  (successive-merge (tag-symbols symbols)))

(define (pairs->leaves symbols)
  (if (null? symbols)
      '()
      (let ((element (car symbols)))
	(adjoin-set (make-leaf (car element)
			       (cadr element))
		    (pairs->leaves (cdr symbols))))))

(define (successive-merge symbols)
  (cond ((null? symbols) '())
	((null? (cdr symbols)) '())
	(else
	 (let ((left (car symbols))
	       (right (cadr symbols)))
	   (cons
	    (make-code-tree
	     left
	     right)
	    (successive-merge (cdr symbols)))))))
