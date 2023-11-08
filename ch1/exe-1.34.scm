(define (square x) (* x x))
(define (f g) (g 2))

(f square)

(f f) ; (f 2) -> (2 2) , 2 is not applicable. This happens because
; f is already bound to the external scope, so it is well defined
; (f f) -> (f 2) -> (2 2)
