(gcd (define a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))



(gcd 206 40)

					; normal order
					; (gcd 206 40)
					; (gcd 40 (remainder 206 40))
					; (gcd 40 6)
					; (gcd 6 (remainder 40 6))
					; (gcd 6 4)
					; (gcd 6 (remainder 6 4))
					; (gcd 6 2)
					; (gcd 2 (remainder 6 2))
					; (gcd 2 0)
					; ;Value: 2
; applicative order -> 18 steps
(gcd
 206
 40)
(gcd
 40
 (remainder
  206
  40))
(gcd
 (remainder 206 40)
 (remainder
  40
  (remainder 206 40)))
(gcd
 (remainder 40 (remainder 206 40))
 (remainder
  (remainder 206 40)
  (remainder 40 (remainder 206 40))))
(gcd
 (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
 (remainder
  (remainder 40 (remainder 206 40))
  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; Value: (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) : 2
