(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; in an applicative-order evaluation the interpreter
; will evaluate p, and enter in an infinite recursive
; call on the other hand in a normal-order evaluation
; the interpreter will not evaluate p and halt
