(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 5 -4)

; if return a primitive procedure either + or -
; depending on the value of b either > 0 or < 0
; therefore it is summing a with the absulute
; value of b
