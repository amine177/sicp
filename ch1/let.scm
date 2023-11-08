(define x 4)
(let ((x 3)
      (y (+ x 2))) ; y <- 4 + 2 . The x here is the one from the 
  (newline)        ; external scope
  (display x)
  (newline)
  (display y)
  (newline)
  (display (+ x y))) ; y: 6, x: 3. The x here is from the interal
                     ; scope of the let expression (3)
