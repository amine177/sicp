(define (f x)
  x)

(define (g f x)
  (if (> x 0)
      0
      (f x)))

(g f (+ 3 1))				; If the interpreter uses normal order evaluation
					; the function is evaluated before the arguments
					; then this function call will return 4.
					; If your interpreter uses applicative order evaluation
					; the function is evaluated after the arguments
					; then the function call will return 0


(define (identity)  (identity))
(define (left-or-right left right)
  (if (= left 0) 0 right))
(left-or-right 0 (identity))		; If the interpreter uses normal order evaluation
					; the function is evaluated before the arguments
					; then this function call will return right
					; If your interpreter uses applicative order evalution
					; the function is evaluated after the arguments
					; then the function call will return 0
