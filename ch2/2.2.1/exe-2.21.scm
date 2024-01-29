(define (square x)
  (* x x))
; without map
(define (square-list lst)
  (if (null? lst)
      '()
      (cons (square (car lst))
	    (square-list (cdr lst)))))

(square-list (list 1 2 3))

; with map
(define (square-list-map lst)
  (map (lambda (x) (* x x)) lst))
(square-list-map (list 1 2 3))
