(define (make-f)
  (define previous 0)

  (define (dispatch darg)
    (begin (let ((ret (* previous darg)))
	     (set! previous darg)
	     ret)))


  dispatch)

(define f (make-f))

(+ (f 0) (f 1))
