(define (get-last-element lst)
  (if (null? (cdr lst))
      (car lst)
      (get-last-element (cdr lst))))

(get-last-element (list 1 2 3 4 7))
