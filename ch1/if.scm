(define (odd x)
  (if (= (modulo x 2) 0)
      false
      true))

(odd 5)
(odd 6)
