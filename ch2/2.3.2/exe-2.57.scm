(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

; sum
(define (make-sum-binary addend augend)
  (cond ((=number? addend 0) augend)
	((=number? augend 0) addend)
	((and (number? addend) (number? augend))
	 (+ addend augend))
	(else (list '+ addend augend))))

; arbitrary sum
(define (make-sum . args)
  (reduce (lambda (x y)
	    (make-sum-binary (make-sum x) y))
	  0
	   args))
	 
	       

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))     ; is x a compound object and
					; is the 1st element of x the
                                        ; symbol +

; product
(define (make-product-binary multiplicand multiplier)
  (cond ((=number? multiplicand 1) multiplier)
	((=number? multiplier 1) multiplicand)
	((or (=number? multiplicand 0)
	     (=number? multiplier 0)) 0)
	((and (number? multiplicand)
	      (number? multiplier))
	 (* multiplicand multiplier))
	(else (list '* multiplicand multiplier))))

; arbitrary product
(define (make-product . args)
  (reduce (lambda (x y)
	    (make-product-binary
	     (make-product x) y))
	  1
	   args))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (caddr p))

;power
(define (fast-exp b n)
  (cond ((= n 0) 1)
	((iseven n)
	 (* (fast-exp b (/ n 2)) (fast-exp b (/ n 2))))
	(else (* b (fast-exp b (- n 1))))))

(define (iseven n)
  (= (remainder n 2) 0))

(define (make-exponant base exponant)
  (cond ((=number? exponant 0) 1)
	((=number? exponant 1) base)
	((and (number? base) (number? exponant))
	 (fast-exp base exponant))
	(else (list '^ base exponant))))

(define (exponant? x)
  (and (pair? x) (eq? (car x) '^)))

(define (base e)
  (cadr e))
(define (exponant e)
  (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	; deriv sum
	((sum? exp) (make-sum (deriv (addend exp) var)
			      (deriv (augend exp) var)))
	;deriv product
	((product? exp) (make-sum
			 (make-product
			  (multiplier exp)
			  (deriv (multiplicand exp) var))
			 (make-product
			  (deriv (multiplier exp) var)
			  (multiplicand exp))))
	;deriv exponant
	((exponant? exp)
	 (make-product
	  (make-product
	   (exponant exp)
	   (make-exponant
	    (base exp)
	    (make-sum
	     (exponant exp)
	     (- 1))))
	  (deriv (base exp) var)))
	
	(else
	 (error
	  "unknown or unsupported expression format: DERIV" exp))))
