(define (exp b n)
  (if (= n 0)
      1
      (* b (exp b (- n 1)))))

(exp 2 2)
(exp 4 2)
(exp 5 3)

(define (exp-t b res n)
  (if (= n 0)
      res
      (exp-t b (* res b) (- n 1))))

(define (exp-it b n)
  (exp-t b 1 n))

(exp-it 4 2)
  
