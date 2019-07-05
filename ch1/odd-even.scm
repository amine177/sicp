(define (odd x)
  (cond ((= (modulo x 2) 0) false)
        (else true)))


(odd 5)
(odd 4)
