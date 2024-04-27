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
(define (magnitude-complex z) (apply-generic 'magnitude z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (real-part z) (apply-generic 'real-part z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (raise x)
  (apply-generic 'raise x))
(define (project x)
  (apply-generic 'project x))
(define (drop x)
  (cond
   ((boolean? x) x)
   ((and (equ? (project x) x)
	 (not (eq? (type-arg x)
		   (type-arg (project x)))))
    (drop (project x)))
   (else x)))

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

; packages -----------------------------------
; ---- integer-package 
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer (lambda (x) (tag (truncate->exact x))))
  (put 'equ? '(integer integer) (lambda (x y) (eq? x y)))
  (put '=zero? '(integer-) (lambda (x) (= x 0)))
  (put 'exp '(integer integer)
       (lambda (x y) (expt x y)))

  ;-- raise & project
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put 'project '(integer)
       (lambda (x) x))
  ;-- coercions
  (put-coercion 'integer 'complex
		(lambda (x) (make-complex-from-real-imag x 0)))
  (put-coercion 'integer
		'integer
		(lambda (n) n)) ; Louis Reasoner addition
  
  'done)

; ---- real-number-package
(define (install-real-package)
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
  (put 'make-real-from-int 'real (lambda (n)
				   (tag (make-real-from-integer n))))
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
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
			    (make-rational
			     (truncate (* x (exp 10 dec-l)))
			     (exp 10 dec-l)))))
  'done)
    
    
; ---- rational-number-package
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cond ((apply-generic 'equ? d 0)
	   (error "null denominator: MAKE-RAT"))
	  (else
	   (let ((g (gcd n d)))
	     (cons
	      (truncate->exact (/ n g)) (truncate->exact (/ d g)))))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))

  (define (equ? x y)
    (and (apply-generic 'equ? (numer x) (numer y))
	 (apply-generic 'equ? (denom x) (denom y))))

  (define (zero? x)
    (apply-generic '=zero? (numer x)))
  
  (define (tag x) (attach-tag 'rational x))

  (put '=zero? '(rational)
       (lambda (x) (zero? x)))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put 'raise '(rational) (lambda (x)
			    (make-real-from-numer-denom
			      (numer  x)
			      (denom x))))
  (put 'project '(rational) (lambda (x)
			      (round->exact (/ (numer x) (denom x)))))
  'done)


; ---- complex number package
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'raise '(rectangular)  (lambda (x) (tag x)))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
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
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (mangnitude z1) (mangnitude z2))
		       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (define (equ? z1 z2)
    (and (apply-generic 'equ? (real-part z1) (real-part z2))
	 (apply-generic 'equ? (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (apply-generic '=zero? (real-part z))
	 (apply-generic '=zero? (imag-part z))))

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
			     (attach-tag 'real (real-part x))))
  (put 'raise '(complex) (lambda (x) (tag x)))

  ;-- coercions
  (put-coercion 'complex 'complex (lambda (z) z))
  'done)

; install the packages
(install-complex-package)
(install-rational-package)
(install-integer-package)
(install-real-package)

(define (type->rank t)
  (cond ((eq? t 'complex) 1)
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
    (cond ((eq? t1 t2) var)
	  ((not (higher? t1 t2))
	     (raise-var-to-type (raise var) type))
	  (else
	   (error
	    "var type higher than type to raise to: RAISE-T1->T2"
	    t1 t2)))))




; apply generic
(define (apply-generic1 op  args)
  (let ((type-args (map type-arg args)))
	(let ((proc (get op type-args)))
	    (if proc
		(apply proc (map contents args))
		(let ((highest-type (get-highest-type
				     type-args)))
		  (let ((type-args-r (map (lambda (x)
					    highest-type)
					  type-args)))
		    (let ((args-r (map (lambda (x)
					 (raise-var-to-type
					  x highest-type))
				       args)))
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
						(list op type-args)))))))
				(error
				 "No method for these types: APPLY-GENERIC"
				 (list op type-args))))))))))))
   
   
(define (apply-generic op . args)
  (cond ((member op (list 'add 'mul 'div 'sub))
	 (drop (apply-generic1 op  args)))
	(else (apply-generic1 op  args))))

;(raise (make-integer 1))

;(raise (raise (make-integer 1)))
;(raise (raise (raise (raise (make-integer 1)))))


(add (make-complex-from-real-imag 1 0)  1)
(mul (make-complex-from-real-imag 1 0)  1)
