(define (new-if pred pred-conseq else-cl)
  (cond (pred pred-conseq)
	(else else-cl)))

(define (sqrt-it guess x)
  (new-if (good? guess x)
	  guess
	  (sqrt-it (refine guess x)
		   x)))

					; Because (new-if ) is a procedure, it will be evaluted
					; using applicative order evaluation
					; that means the interpreter will try to execut both (good? ) and (sqrt-it ) before substituting (new-if ) for its expanded form
					; the execution of (sqrt-it ) will invoke another (new-if ) -> another call to (sqrt-it )
					; -> (new-if ) -> (sqrt-it ) ...
; which will cause the stack to be full and therfore reaching the maximum recursion depth
