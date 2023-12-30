(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
	((not (pair? mobile)) mobile)
	((and (pair? mobile) (not (pair? (branch-structure mobile))))
	 (branch-structure mobile))
	(else
	 (+ (total-weight (branch-structure (right-branch mobile)))
	    (total-weight (branch-structure (left-branch mobile)))))))

(total-weight (make-mobile (make-branch 5 (make-branch 2 10)) (make-branch 7 4)))
(total-weight (make-mobile (make-branch 5 10) (make-branch 7 4)))
