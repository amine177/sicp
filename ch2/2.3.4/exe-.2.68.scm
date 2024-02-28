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
			 (decode-1 (cdr bits) tree))
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

(define (contains-list? e set)
  (cond ((null? set) #f)
	((eq? e (car set)) #t)
	(else (contains-list? e (cdr set)))))

(define (contains? symbol tree)
  (contains-list? symbol (symbols tree)))

(define (in-left? symbol tree)
  (contains? symbol (left-branch tree)))


(define (in-tree? symbol tree)
  (cond ((null? tree) #f)
	(else
	 (contains? symbol (symbols tree)))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (cond ((contains? symbol tree)
	     (if (in-left? symbol tree)
		 (append (list '0)
			(encode-symbol symbol (left-branch tree)))
		(append (list '1)
			(encode-symbol symbol (right-branch tree)))))
	    (else (error
		   "symbol not found in tree: ENCODE-SYMBOL"
		   symbol)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))


(define (pairs->leaves symbols)
  (if (null? symbols)
      '()
      (let ((element (car symbols)))
	(adjoin-set (make-leaf (car element)
			       (cadr element))
		    (pairs->leaves (cdr symbols))))))

(define (generate-huffman-tree symbols)
  (successive-merge (pairs->leaves symbols)))

(define (successive-merge symbols)
  (display "\nsymbols: ")
  (display symbols)
  (cond ((null? symbols) '())
	((null? (cdr symbols)) (car symbols))
	(else
	 (let ((left (car symbols))
	       (right (cadr symbols)))
	   (if (null? (cddr symbols))
	       (make-code-tree left right)
	       (make-code-tree (make-code-tree left right)
			       (successive-merge
				(cddr symbols))))))))

(define (generate-huffman-tree-iter symbols)
  (successive-merge-iter (pairs->leaves symbols)))

(define (successive-merge-iter symbols)
  (define (iter result symbols)
    (cond ((null? symbols) result)
	  ((not (pair? (cdr symbols))) (make-code-tree result
						      (car symbols)))
	(else
	 (let ((left (car symbols))
	       (right (cadr symbols)))
	   (if (null? result)
	       (iter (make-code-tree left
				     right)
		     (cddr symbols))
	       (iter (make-code-tree left
				     result)
		     (cdr symbols)))))))
  (iter '() symbols))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree
		    (make-leaf 'D 1)
		    (make-leaf 'C 1)))))
 

(decode (encode '( A B C) (generate-huffman-tree-iter '( ( A 10) (B 1) (C 2))))  (generate-huffman-tree '( ( A 10) (B 1) (C 2))))

(decode (encode '( A B C) (generate-huffman-tree '( ( A 10) (B 1) (C 2))))  (generate-huffman-tree-iter '( ( A 10) (B 1) (C 2))))

(decode (encode '( A B C) (generate-huffman-tree '( ( A 10) (B 1) (C 2))))  (generate-huffman-tree '( ( A 10) (B 1) (C 2))))

(decode (encode '( A B C) (generate-huffman-tree-iter '( ( A 10) (B 1) (C 2))))  (generate-huffman-tree-iter '( ( A 10) (B 1) (C 2))))
