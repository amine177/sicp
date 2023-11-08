(define x 4)
(let ((x 3)
      (y (+ x 2))) ; y <- 4 + 2
  (newline)
  (display x)
  (newline)
  (display y)
  (newline)
  (display (+ x y))) ; y: 6, x: 3
