(define list1 (list 1 2 3 45))
(newline)
(display (car list1))
(newline)
(display (cdr list1))
(newline)

(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (length-iter lst ctr)
  (if (null? lst)
      ctr
      (length-iter (cdr lst) (+ ctr 1))))


(list-ref (list 5 10 20) 0)
(list-ref (list 5 10 20) 1)
(list-ref (list 5 10 20) 2)
(length (list 3 10 11))
(length-iter (list 3 10 11) 0)
