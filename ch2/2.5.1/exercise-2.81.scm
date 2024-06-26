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
	((number? datum) 'scheme-number)
	(else (error
	       "datum should be '(type contents) or number : TYPE-ARG"
	     datum))))
(define (contents datum)  
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else (error "datum should be '(type contents): CONTENTS"
	     datum))))
(define (attach-tag tag var)
  (if (number? var)
      var
      (cons tag var)))


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

; constructors
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; packages -----------------------------------
; ---- scheme-number-package 
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (eq? x y)))
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (expt x y)))
  ;-- coercions
  (put-coercion 'scheme-number 'complex
		(lambda (x) (make-complex-from-real-imag x 0)))
  (put-coercion 'scheme-number
		'scheme-number
		(lambda (n) n)) ; Louis Reasoner addition
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
	     (cons (/ n g) (/ d g))))))
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
  (define (div-art x y)
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
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (mangitude z) (sin (angle z))))
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

  ;-- coercions
  (put-coercion 'complex 'complex (lambda (z) z))
  'done)

; install the packages
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

;--- a. Before Louis Reasoner addition, we get the usual
; error :
;No method for these type (exp (complex complex))
; With Louis's coercion procedures installed
; We get a call to apply-generic using the idenity
; proecude to coerce one of the arguments.
; Which results in the same original call over and over.
(exp (make-complex-from-real-imag 1 2)
     (make-complex-from-real-imag 1 2))

;--- b.
; No Louis is not correct, because those procedures
; can be installed as part of the package of that type.
; It is unecessary to coerce a type to itself.
; What might be a good generalization is coercing a type
; to another type for which apply-generic will halt.
; So we need to either install with PUT a procedure that
; computes on that type or a coercion that halts.

;--- c.
(define (apply-generic op . args)
  (let ((type-args (map type-arg args)))
    (let ((proc (get op type-args)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-args))
		    (type2 (cadr type-args))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (eq? type1 type2)
		    (error "No method for these types"
			   (list op type-args))
		    (let ((t1->t2 (get-coercion type1
						type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
 			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else (error "No method for these types"
					 (list op type-args)))))))
		(error "No method for these types: APPLY-GENERIC"
		       (list op type-args)))))))
