(define a 1)
(define b 2)
(display "a list without quotation: ")
(list a b)
(newline)

(display "a list with 'a and 'b: ")
(list 'a 'b)
(newline)

(car '(a b c))
(cdr '(a b c))

; returns false or the list begining with x
(define (memq x lst)
  (cond ((null? lst) false)
	((eq? x (car lst)) lst)
	(else (memq x (cdr lst)))))
