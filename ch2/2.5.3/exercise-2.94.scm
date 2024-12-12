(define *op-table* (make-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

(define *coercion-table* (make-hash-table))
(define (put-coercion stype
		      dtype
		      proc)
  (hash-table/put! *coercion-table*
		   (list stype
			 dtype)
		   proc))
(define (get-coercion stype
		      dtype)
  (hash-table/get *coercion-table* (list stype dtype) #f))

(define (type-arg datum)
  (cond ((pair? datum) (car datum))
	((not (exact? datum)) 'real)
	((integer? datum) 'integer)
	(else (error
	       "datum should be '(type contents) or number : TYPE-ARG"
	     datum))))
(define (contents datum)  
  (cond ((pair? datum) (cdr datum))
	((not (exact? datum)) datum)
	((integer? datum) datum)
	(else (error "datum should be '(type contents): CONTENTS"
	     datum))))
(define (attach-tag tag var)
  (cond ((and
	  (number? var )
	  (not (exact? var)))
	 (* 1.0 var))
	((number? var) var)
	(else (cons tag var))))


; interfaces to the packages
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (gcd-generic x y)
  (cond ((or (pair? x) (pair? y))
	 (apply-generic 'gcd x y))
	(else
	 (apply gcd (list x y)))))
(define (make-sparse-polynomial var terms)
  ((get 'make-sparse-polynomial 'polynomial) var terms))
(define (make-dense-polynomial var terms)
  ((get 'make-dense-polynomial 'polynomial) var terms))
(define (magnitude-complex z) (apply-generic 'magnitude z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (real-part z) (apply-generic 'real-part z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y)
      (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (raise x)
  (apply-generic 'raise x))
(define (project x)
  (apply-generic 'project x))
(define (drop x)
  (if (boolean? x)
      x
      (let ((proj  (project x)))
	(cond
	 ((and (equ? proj x)
	 (not (eq? (type-arg x)
		   (type-arg proj))))
	  (drop proj))
	 (else x)))))

; constructors
(define (make-integer n)
  ((get 'make 'integer) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (make-real-from-integer n)
  ((get 'make-real-from-integer 'real) n))
(define (make-real-from-numer-denom n d)
  ((get 'make-real-from-numer-denom 'real) n d))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; packages -----------------------------------
; ---- integer-package 
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'neg '(integer)
       (lambda (x)
	 (- x)))
  (put 'add '(integer integer)
       
       (lambda (x y)
	 (tag (+ x y))))
  (put 'sub '(integer integer)
       
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       
       (lambda (x y) (tag (truncate->exact (/ x y)))))
  (put 'make 'integer (lambda (x) (tag (truncate->exact x))))
  (put 'equ? '(integer integer) (lambda (x y) (eq? x y)))
  (put '=zero? '(integer) (lambda (x) (= x 0)))
  (put 'exp '(integer integer)
       (lambda (x y) (expt x y)))

  (put 'sine '(integer) (lambda (x)
			   (apply-generic 'sine
					  (raise (make-integer x)))))
  (put 'cosine '(integer) (lambda (x)
			     (apply-generic 'cosine
					    (raise (make-integer x)))))
  (put 'atan '(integer integer) (lambda (x y)
				   (apply-generic
				    'atan
				    (raise (make-integer x))
				    (raise (make-integer y)))))
  (put 'sqrt '(integer) (lambda (x)
			      (apply-generic 'sqrt
					     (raise x))))
  (put 'gcd '(integer integer) (lambda (x y)
				 (gcd x y)))

  ;-- raise & project
  (put 'raise '(integer)
       (lambda (x)
	 (make-rational x 1)))
  (put 'project '(integer)
       (lambda (x)  x))
  ;-- coercions
  (put-coercion 'integer 'complex
		(lambda (x) (make-complex-from-real-imag x 0)))
  (put-coercion 'integer
		'integer
		(lambda (n) n)) ; Louis Reasoner addition  
  'done)

; ---- real-number-package
(define (install-real-package)
  (put 'neg '(real)
       (lambda (x)
	 (- x)))
  (define (get-decimal-length x)
    (define (find-dot strarr)
      (cond ((null? strarr) 0)
	    ((eq? (car strarr) #\.) (length (cdr strarr)))
	    (else (find-dot (cdr strarr)))))
    (find-dot (string->list (number->string x))))
  (define (tag x)
    (attach-tag 'real x))
  (define (make-real-from-integer n)
    (* 1.0 n))
  (define (make-real-from-numer-denom n d)
    (cond ((apply-generic 'equ? d 0)
	   (error "null denominator: MAKE-REAL-FROM-NUMER-DENOM"))
	  ((= n 0) 0.0)
	  (else (/ (make-real-from-integer n)
		   (make-real-from-integer d)))))
  (define (sine x)
    (sin x))
  (define (cosine x)
    (cos x))


  (put 'sine '(real) (lambda (x)
		     (tag (sine x))))
  (put 'cosine '(real) (lambda (x)
			 (tag (cosine x))))
  (put 'atan '(real real) (lambda (x y)
			    (tag (atan x y))))
  (put 'sqrt '(real)
       (lambda (x)
	 (tag (sqrt x))))


  
  (put 'make-real-from-int 'real (lambda (n)
				   (tag (make-real-from-integer n))))
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put 'add '(real real) (lambda (x y)
			   (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y)
			   (tag (- x y))))
  (put 'div '(real real) (lambda (x y)
			   (tag (/ x y))))
  (put 'mul '(real real) (lambda (x y)
			   (tag (* x y))))
  (put 'exp '(real real) (lambda (x y)
			   (tag (expt x y))))
  (put
   'make-real-from-numer-denom
   'real
   (lambda (n d)
     (tag
      (make-real-from-numer-denom n d))))

  ; -- project & raise
  (put 'raise '(real) (lambda (x)
			(make-complex-from-real-imag x 0.0)))
  (put 'project '(real) (lambda (x)
			  (let ((dec-l (get-decimal-length x)))
			    (let ((dec-denom-rs (apply-generic 'exp 10 dec-l)))
			    (make-rational
			     (truncate (* x dec-denom-rs))
			     dec-denom-rs)))))
  'done)
    
    
; ---- rational-number-package
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (make-rat n d)
    (cond ((apply-generic '=zero? d)
	   (error "null denominator: MAKE-RAT"))
	  (else
	   (let ((g (gcd-generic n d)))
	     (cond ((not (and (pair? n) (pair? d)))
		    (cons
		     (truncate->exact (div n g)) (truncate->exact (div d g))))
		   (else
;		    (display "\nNumer ")
;		    (display (apply-generic 'div n g))
;		    (display "\nDenom ")
		    (display (apply-generic 'div d g))
		    
				       
		    (cons (apply-generic 'div n g) (apply-generic 'div d g))))))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
		 (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
		 (mul (numer y) (denom x)))
	      (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
	      (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
	      (mul (denom x) (numer y))))

  (define (equ? x y)
    (and (apply-generic 'equ? (numer x) (numer y))
	 (apply-generic 'equ? (denom x) (denom y))))

  (define (zero? x)
    (apply-generic '=zero? (numer x)))
  
  (define (tag x) (attach-tag 'rational x))
  (define (exp x y)
    (tag
    (apply-generic 'exp
		   (raise (make-rational (numer x)
					      (denom x)))
		   (raise (make-rational (numer y)
					 (denom y))))))

  (put 'neg '(rational)
       (lambda (x)
	 (make-rational
	  (- (numer x))
	  (denom x))))
	    
  (put 'exp '(rational rational) (lambda (x y)
				   (tag (exp x y))))
  (put 'sine '(rational) (lambda (x)
	
			   (apply-generic 'sine
			    (raise (make-rational (numer x)
						  (denom x))))))
  (put 'atan '(rational rational) (lambda (x y)
				   (apply-generic
				    'atan (raise (make-rational
						  (numer x)
						  (denom x)))
				    (raise (make-rational
					    (numer y)
					    (denom y))))))
					    
  (put 'atan '(rational) (lambda (x)
			   (apply-generic 'atan
					       (raise (make-rational
						       (numer x)
						       (denom x))))))
  (put 'cosine '(rational) (lambda (x)
			   (apply-generic 'cosine
			    (raise (make-rational (numer x)
						  (denom x))))))
  (put 'sqrt '(rational) (lambda (x)
			      (apply-generic 'sqrt
					     (raise (make-rational
						     (numer x)
						     (denom x))))))
  (put '=zero? '(rational)
       (lambda (x) (zero? x)))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put 'raise '(rational) (lambda (x)
			    (make-real-from-numer-denom
			      (numer  x)
			      (denom x))))

  (put 'project '(rational) (lambda (x)

			      (cond ((or
				      (pair? (numer x)) (pair? (denom x)))
				     (tag x))
				    (else
				     (let ((fraction (div (numer x) (denom x))))
				       (cond ((not (pair? fraction))
					      (round->exact fraction))
					     (else
					      fraction)))))))
  'done)


; ---- complex number packages
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude-rect z)
    (apply-generic 'sqrt (apply-generic 'add
					(apply-generic
					 'mul
					  (real-part z)
					  (real-part z))
					(apply-generic
					 'mul
					 (imag-part z)
					 (imag-part z)))))
  (define (angle z)
    (apply-generic 'atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  

  (define (tag x) (attach-tag 'rectangular x))
  
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude-rect)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'raise '(rectangular)  (lambda (x) (tag x)))
  'done)

; polar package
(define (install-polar-package)
  (define (magnitude-polar z)
    (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (apply-generic 'mul
		   (apply-generic 'magnitude (tag z))
		   (apply-generic 'cosine (angle z))))
  (define (imag-part z)
    (apply-generic
     'mul
     (apply-generic 'magnitude (tag z))
     (apply-generic 'sine (angle z))))
 
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  

  (define (tag x) (attach-tag 'polar x))
  
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude-polar)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'raise '(polar) (lambda (x) (tag x)))
  'done)

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (apply-generic
			  'add
			  (real-part z1)
			  (real-part z2))
			 (apply-generic
			  'add
			  (imag-part z1)
			  (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (apply-generic
			  'sub
			  (real-part z1)
			  (real-part z2))
			 (apply-generic
			  'sub (imag-part z1) (imag-part z2))))
  
  (define (mul-complex z1 z2)
    (make-from-real-imag (apply-generic 'sub
					(apply-generic 'mul
					(real-part z1)
					(real-part z2))
					(apply-generic 'mul
					(imag-part z1)
					(imag-part z2)))
			 (apply-generic 'add
					(apply-generic 'mul
						       (real-part z1)
						       (imag-part z2))
					(apply-generic 'mul
						       (imag-part z1)
						       (real-part z2)))))
			 

  (define (div-complex z1 z2)
    (make-from-real-ang (apply-generic 'div (magnitude z1) (magnitude z2))
		       (apply-generic 'sub (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (define (equ? z1 z2)
    (and (apply-generic 'equ? (real-part z1) (real-part z2))
	 (apply-generic 'equ? (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (apply-generic '=zero? (real-part z))
	 (apply-generic '=zero? (imag-part z))))

  (put 'neg '(complex)
       (lambda (x)
	 (make-from-real-imag (- (real-part x))
			      (- (imag-part x)))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z) (=zero? z)))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'project '(complex) (lambda (x)
			     (real-part x)))
  (put 'raise '(complex) (lambda (x) (tag x)))

  ;-- coercions
  (put-coercion 'complex 'complex (lambda (z) z))
  'done)



(define (type->rank t)
  (cond
   ((eq? t 'polynomial) 0)
   ((eq? t 'dense-polynomial) 0)
   ((eq? t 'sparse-polynomial) 0)
   ((eq? t 'complex) 1)
   ((eq? t 'rectangular) 1)
   ((eq? t 'polar) 1)
   ((eq? t 'real) 2)
   ((eq? t 'rational) 3)
   ((eq? t 'integer) 4)
   ((eq? t 'nil) 100)))

(define (higher? t1 t2)
  (< (type->rank t1) (type->rank t2)))

(define (max t1 t2)
  (if (higher? t1 t2)
      t1
      t2))
  
(define (get-highest-type types)
  (fold-right max 'nil types))

(define (raise-var-to-type var type)

  
  (let ((t2 type)
	(t1 (type-arg var)))
    (cond
     ((eq? (type-arg (raise var)) (type-arg var)) var)
     ((eq? t1 t2) var)
     ((not (higher? t1 t2))
      (raise-var-to-type (raise var) type))
     (else
	   (error
	    "var type higher than type to raise to: RAISE-T1->T2"
	    t1 t2)))))



; apply generic
(define (apply-generic1 op . args)
  (let ((type-args (map type-arg args)))
	(let ((proc (get op type-args)))
	    (if proc
		(apply proc (map contents args))
		(let ((highest-type (get-highest-type
				     type-args)))

		  
		    (let ((args-r (map (lambda (x)
					 (raise-var-to-type
					  x highest-type))
				       args)))
		      (let ((type-args-r (map type-arg
					      args-r)))

		      (let ((proc (get op type-args-r)))
			(if proc
			    (apply proc (map contents args-r))
			    (if (= (length args) 2)
				(let ((type1 (car type-args))
				      (type2 (cadr type-args))
				      (a1 (car args))
				      (a2 (cadr args)))
				  (if (eq? type1 type2)
				      (error
				       "No method for these types"
					     (list op type-args))
				      (let ((t1->t2 (get-coercion
						     type1
						     type2))
					    (t2->t1 (get-coercion
						     type2
						     type1)))
					(cond (t1->t2
					       (apply-generic1 op
							      (t1->t2
							       a1)
							      a2))
 					      (t2->t1
					       (apply-generic1 op
							      a1
							      (t2->t1
							       a2)))
					 (else
					   (error
					   "No method for these types"
					   (list op type-args)
					   )
					   )))))
				(error
				 "No method for these types: APPLY-GENERIC"
				 (list op type-args) )))))))))))
   
(define (apply-generic op . args)
  (cond ((member op (list 'add 'mul 'div 'sub  ))
	 (let ((result (apply apply-generic1 op  args)))

	   (cond ((and (pair? result)
		       (eq? (car result) 'polynomial-fraction))
		  result)
		 (else (drop result)))))

	(else (apply apply-generic1 op  args))))

(define (install-dense-polynomial-package)
  (define (tag p)
    (attach-tag 'dense-polynomial p))
  (define (make-poly var term-list)
    (cons var term-list))
  (define (same-variable? v1 v2)
    (eq? v1 v2))
;  (trace same-variable?)
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))
  (define (empty-termlist? t)
    (null? t))
  (define (the-empty-termlist) '())
  (define (adjoin-term term term-list)
    (cons term term-list))
  (define (first-term l)
    (car l))
  (define (rest-terms l)
    (cdr l))
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
	  ((empty-termlist? l2) l1)
	  (else (adjoin-term (apply-generic 'add
				      (car l1)
				      (car l2))
			     (add-terms (cdr l1) (cdr l2))))))
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
	  (the-empty-termlist)
	  (add-terms (mul-term-by-all-terms  (first-term l1) l2)
		     (cons 0 (mul-terms (rest-terms l1) l2)))))
  (define (mul-term-by-all-terms t l)
    (if (empty-termlist? l)
	(the-empty-termlist)
	(adjoin-term (apply-generic 'mul t (first-term l))
		     (mul-term-by-all-terms t (rest-terms l)))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly
	 (variable p1)
	 (add-terms (term-list p1) (term-list p2)))
	(error
	 "Polynomials not in same variable - ADD-POLY - DENSE")))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error
	 "Polynomials not in same var - MUL-POLY - DENSE")))
  (define (neg p)
    (make-poly (variable p)
	       (map (lambda (x)
		      (apply-generic 'neg x))
		    (term-list p))))
  (define (sub p1 p2)
    (apply-generic 'add p1 (apply-generic 'neg p2)))
  (define (same-term-list l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((not (= (length l1) (length l2))) #f)
	  (else
	   (and (apply-generic 'equ?
			       (first-term l1)
			       (first-term l2))
		(same-term-list (rest-terms l1)
				(rest-terms l2))))))
  (define (equ? p1 p2)
    (and (same-variable? (variable p1) (variable p2))
	 (same-term-list (term-list p1)
			 (term-list p2))))
  
  (put 'add '(dense-polynomial dense-polynomial)
       (lambda (p1 p2)
	 (tag (add-poly p1 p2))))
  (put 'mul '(dense-polynomial dense-polynomial)
       (lambda (p1 p2)
	 (tag (mul-poly p1 p2))))
  (put 'sub '(dense-polynomial dense-polynomial)
       (lambda (p1 p2)
	 (sub p1 p2)))
  (put 'make 'dense-polynomial
       (lambda (var term-list)
	 (tag (make-poly var term-list))))
  (put 'project '(dense-polynomial)
       (lambda (p)  (tag p)))
  (put 'equ? '(dense-polynomial dense-polynomial)
       (lambda (p1 p2)
	 (equ? p1 p2)))
  (put 'raise '(dense-polynomial)
       (lambda (p)
	 (tag p)))
  'done)

(define (install-sparse-polynomial-package)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (higher-var? x y)
    (symbol<? x y))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (tag p) (attach-tag 'sparse-polynomial p))
  (define (mul-poly-integer p i)
    (make-poly (variable p)
	       (mul-term-by-int (term-list p) i)))
  (define (mul-term-by-int l i)
    (if (empty-termlist? l)
	(the-empty-termlist)
	(let ((t (first-term l)))
	  (adjoin-term (make-term (order t)
				  (apply-generic 'mul (coeff t) i))
		       (mul-term-by-int (rest-terms l) i)))))
  
  (define (remainder-terms a b)
    (cadr (div-terms a  b)))
  
;  (define (gcd-terms a b)
;    (display "\n B: ")
;    (display b)
;    (display "\n") 
;    (cond ((empty-termlist? b)
;	   a)
;	  (else (gcd-terms b (remainder-terms a b)))))
  (define (gcd-terms a b)
    (cond ((empty-termlist? b)
	   a)
	  (else
;	   (display "B is not empty\n")
	   (let ((remainder (remainder-terms a b)))
;	     (display "GCD-TERMS, REMAINDER: ")
;	     (display remainder)
;	     (display "\n")
	     (cond ((and (pair? remainder)
			 (=zero? (order (first-term remainder))))
		    (list (make-term 0 1)))
		   (else
		    (cond ((terms-list-equal? a remainder)
		    	   (list (make-term 0 1)))
		       (else
			(gcd-terms b (remainder-terms a b))))))))))
;  (trace gcd-terms)

  (define (gcd-polys a b)
    (cond ((not (same-variable? (variable a) (variable b)))
	   (error "Polynomials not in the same variable"))
	  (else (gcd-terms (term-list a)
			   (term-list b)))))
;  (trace gcd-polys)
	  
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
	  ((empty-termlist? l2) l1)
	  (else
	   (let ((t1 (first-term l1))
		 (t2 (first-term l2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term t1 (add-terms (rest-terms l1)
					       l2)))
		   ((< (order t1) (order t2))
		    (adjoin-term t2 (add-terms l1
					       (rest-terms l2))))
		   (else (adjoin-term
			  (make-term
			   (order t1)
			   (add (coeff t1) (coeff t2)))
			  (add-terms (rest-terms l1)
				     (rest-terms l2)))))))))

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term l1)
					  l2)
		   (mul-terms (rest-terms l1) l2))))
  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
	(the-empty-termlist)
	(let ((t2 (first-term l)))
	  (adjoin-term
	   (make-term (add (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms l))))))
  (define (make-poly variable term-list) (cons variable term-list))
  (define (same-variable? v1 v2)
    (eq? v1 v2))
;  (trace same-variable?)
  (define (variable p) (car p))
  (define (term-list p)
    (cdr p))

  (define (add-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
	  (make-poly (variable p1)
		     (add-terms (term-list p1) (term-list p2)))
	  (cond ((higher-var? (variable p1) (variable p2))
		 (add-poly p1
			   (make-poly (variable p1)
				      (list (list 0 (tag (make-poly (variable p2)
							       (term-list p2))))))))
		(else (add-poly p2
			   (make-poly (variable p2)
				      (list (list 0 (tag (make-poly (variable p1)
							       (term-list p1)))))))))))
		
  (define (mul-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
	  (make-poly (variable p1)
		     (mul-terms (term-list p1) (term-list p2)))
	  (cond ((higher-var? (variable p1) (variable p2))
		 (mul-poly p1 (make-poly
			       (variable p1)
			       (list (list 0 (tag (make-poly (variable p2)
							     (term-list p2))))))))
		(else (mul-poly
		       p2
		       (make-poly
			(variable p2)
			(list (list 0 (tag (make-poly (variable p1)
						      (term-list p1)))))))))))
		
;	  (error "Polynomials not in the same variable: MUL-POLY"
;		 (list p1 p2))))


  (define (terms-list-equal? l1 l2)
    (cond ((empty-termlist? l1)
	   (eq? l1 l2))
	  ((empty-termlist? l2)
	   #f)
	  (else (and (terms-equal? (car l1)
				   (car l2))
		     (terms-list-equal? (cdr l1)
					(cdr l2))))))

  (define (div-terms l1 l2)
;    (display "\n L1 :")
;    (display l1)

      (if (empty-termlist? l1)
	  (list (the-empty-termlist)
		(the-empty-termlist))
	  (let ((t1 (first-term l1))
	       (t2 (first-term l2)))
	      (if (> (order t2) (order t1))
		  (list (the-empty-termlist) l1)
		  (let ((new-c (div (coeff t1) (coeff t2)))
			(new-o (- (order t1) (order t2))))

		    (let ((addition-result
			   (add-terms
			    l1
			    (map (lambda (t)
				    (neg-term t))
				 (mul-term-by-all-terms
				  (make-term new-o
					     new-c)
				  l2))
			    
			    )))
;		      (display "ADDITION-RESULT ")
;		      (display addition-result)
;		      (display "\n")
		      (if (terms-list-equal? l1 addition-result)
			  (list (adjoin-term (make-term
					new-o
					new-c)
					     '())
				addition-result)
		      (let ((rest-of-result
			   (div-terms
			    addition-result l2)))
;			(display "\n rest-of-result ")
;			(display rest-of-result)
		      		      
		      (list (adjoin-term (make-term
					  new-o
					  new-c)
					 (car rest-of-result))
			    (cadr rest-of-result))))))))))
;  (trace div-terms)




    
					;    (trace div-terms)

    (define (div-poly p1 p2)
      (cond ((same-variable? (variable p1) (variable p2))
	  (let ((result
		 (map (lambda (t)
			(cond ((null? t)
			       (make-poly (variable p1) (list (list 0 0))))
			      (else
			       (make-poly (variable p1) t))))
		      (div-terms (term-list p1) (term-list p2)))))
		  (list (car result)
			(cadr result))))
	    (else
	     (error "Polynomials not in the same variable: DIV-POLY"
		 (list p1 p2)))))
   
;   (trace div-poly)

    (define (terms-equal? t1 t2)
      (and (apply-generic 'equ? (order t1) (order t2))
	   (apply-generic 'equ? (coeff t1) (coeff t2))))
    (define (same-terms? l1 l2)
      (cond ((and (null? l1) (null? l2)) #t)
	    ((not (= (length l1) (length l2))) #f)
	    (else
	     (and
	      (terms-equal? (first-term l1)  (first-term l2))
	      (same-terms? (rest-terms l1) (rest-terms l2))))))
	     
    (define (equ? p1 p2)
      (and (same-variable? (variable p1) (variable p2))
	   (same-terms? (term-list p1) (term-list p2))))
    (define (zero? p)
      (=
       (length
	(filter
	 (lambda (t)
	   (not (apply-generic '=zero? (coeff t))))
	 (term-list p))) 0))

    (define (neg-term t)
      

      (make-term (order t)
	    (apply-generic 'neg (coeff t))))
;    (trace neg-term)
    (define (neg p)
      (make-poly (variable p)
		 (map  (lambda (t)
			 (neg-term t))
		       (term-list p))))

      

    ;; interface
    (put 'neg '(sparse-polynomial)
	 (lambda (p)
	   (tag (neg p))))
    (put 'add '(sparse-polynomial sparse-polynomial)
	 (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'sub '(sparse-polynomial sparse-polynomial)
	 (lambda (p1 p2)
	   (tag (add-poly p1
			  (neg p2)))))
    (put 'div '(sparse-polynomial sparse-polynomial)
	 (lambda (p1 p2)
	   (let ((result
		  (div-poly p1
			    p2)))
	     (let ((quotient (car result))
		   (remainder (cadr result)))
	       (cond ((zero? remainder)
		      (tag quotient))
		     (else
		      
		      
			   (cons 'polynomial-fraction (list (tag quotient)
				  (tag remainder)))))))))
    (put 'mul '(sparse-polynomial sparse-polynomial)
	 (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'mul '(sparse-polynomial integer)
	 (lambda (p i) (tag (mul-poly-integer p i))))
    (put 'mul '(integer sparse-polynomial)
	 (lambda (i p) (tag (mul-poly-integer p i))))
    (put 'make 'sparse-polynomial
	 (lambda (var terms) (tag (make-poly var terms))))
    (put 'equ? '(sparse-polynomial sparse-polynomial)
	 (lambda (p1 p2)
	   (equ? p1 p2)))
    (put 'equ? '(polynomial-fraction polynomial-fraction)
	 (lambda (f1 f2)
	   (and (apply-generic 'equ? (car f1) (car f2))
		(apply-generic 'equ? (cadr f1) (cadr f2)))))
		
    (put 'project '(sparse-polynomial)
	 (lambda (x) (tag x)))
    (put '=zero? '(sparse-polynomial)
	 (lambda (p) (zero? p)))
    (put 'raise '(sparse-polynomial)
       (lambda (p)
	 p))
    (put 'gcd '(sparse-polynomial sparse-polynomial)
	 (lambda (a b)

	   (tag (make-poly (variable a) (gcd-polys a b)))))
;    (put 'project '(sparse-polynomial)
;	 (lambda (p) p))
    'done)

(define (install-polynomial-package)
  (define (term-list p)
    (cdr p))
  (define (length-to-list l i)
    (cond ((= 0 l) '())
	  (else
	   (cons i (length-to-list (- l 1) (+ i 1))))))
  (define (dense-terms-list-to-sparse l)
    (reverse (filter (lambda (t)
	      (not (eq? (cadr t) 0)))
	    (map (lambda (q p)
		   (list q p))
		 (length-to-list (length l) 0)
	 l))))
	   


  (define (convert-to-sparse p)
    (let ((type-p (car p)))
      (cond ((eq? type-p 'sparse-polynomial)
	     p)
	    ((eq? type-p 'dense-polynomial)
	     (cons 'sparse-polynomial
		   (cons
		    (cadr p)
		    (dense-terms-list-to-sparse
		     (cddr p))))))))
;  (trace convert-to-sparse)
		    
	     
    
    
  (install-sparse-polynomial-package)
  (install-dense-polynomial-package)
  (define (tag x) (attach-tag 'polynomial x))
  (define (make-sparse-poly var term-list)
    ((get 'make 'sparse-polynomial) var term-list))
  (define (make-dense-poly var term-list)
    ((get 'make 'dense-polynomial) var term-list))
  (define (add-poly p1 p2)
    (apply-generic 'add p1 p2))
  (define (sub-poly p1 p2)
    (apply-generic 'sub p1 p2))
  (define (mul-poly p1 p2)
    (apply-generic 'mul p1 p2))
  (define (div-poly p1 p2)
    (apply-generic 'div p1 p2))
  (define (=zero? p)
    (apply-generic '=zero? p))
  (define (project p)
;    (display "PROJECT POLY")
    (apply-generic 'project p))
  (define (equ? p1 p2)
    (apply-generic 'equ? p1 p2))
  (define (neg p)
    (apply-generic 'neg p))

  
  (put 'make-sparse-polynomial 'polynomial
       (lambda (var term-list)
	 (tag (make-sparse-poly var term-list))))
  (put 'make-dense-polynomial 'polynomial
       (lambda (var term-list)
	 (tag (make-dense-poly var term-list))))
  (put 'make-polynomial 'polynomial
       (lambda (var term-list)
	 (tag (make-sparse-polynomial var term-list))))
  (put 'add '(polynomial polynomial)
       (lambda ( p1 p2)
		(tag  (add-poly
		       (convert-to-sparse p1)
		       (convert-to-sparse p2)))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
	 (tag (mul-poly (convert-to-sparse p1)
			(convert-to-sparse p2)))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
	 (div-poly (convert-to-sparse p1)
			(convert-to-sparse p2))))
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2)
	 (equ? p1 p2)))
  (put '=zero? '(polynomial)
       (lambda (p)
	 (=zero? p)))
  (put 'neg '(polynomial)
       (lambda (p)
	 (tag (neg p))))
  (put 'project '(polynomial)
       (lambda (p) (tag p)))
  (put 'raise '(polynomial)
       (lambda (p) (tag p)))
  (put 'gcd '(polynomial polynomial)
       (lambda (a b) (tag (apply-generic 'gcd (convert-to-sparse a)
					 (convert-to-sparse b)))))
  (put 'make 'polynomial
       (lambda (var term-list)
	 (tag (make-sparse-poly var term-list))))
       
  'done)


; install the packages
(install-complex-package)
(install-rational-package)
(install-integer-package)
(install-real-package)
(install-polynomial-package)
;(trace apply-generic)
;(trace apply-generic1)




(define p1 (make-sparse-polynomial 'x '((2 1) (0 1))))

(define p2 (make-sparse-polynomial 'x '((3 1) (0 1))))

(define r (make-rational p2 p1))
