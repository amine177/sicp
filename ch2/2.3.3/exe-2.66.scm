(define (key db)
  (entry db))

(define (lookup record-key db)
  (cond ((null? db) #f)
	((equal? (key db) record-key) db)
	((> (key db) record-key)
	 (lookup record-key (left-branch db)))
	(else
	 (lookup record-key (right-branch db)))))

(lookup 1 '(1 (2 () ()) (3 () ())))
