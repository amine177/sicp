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
	(else
	 (+ (total-weight (branch-structure (right-branch mobile)))
	    (total-weight (branch-structure (left-branch mobile)))))))


(define (balanced? mobile)
  (cond ((null? mobile) #t)
	((not (pair? mobile)) #t)
	(else
	 (and
	  (=
	   (* (branch-length (right-branch mobile))
	      (total-weight (branch-structure (right-branch mobile))))
	   (* (branch-length (left-branch mobile))
	      (total-weight (branch-structure (left-branch mobile)))))
	  (balanced? (branch-structure (right-branch mobile)))
	  (balanced? (branch-structure (left-branch mobile)))))))



(balanced? (make-mobile (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 2 2))) (make-branch 2 2)))
(balanced? (make-mobile (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 2))) (make-branch 2 2)))

; cons instead of list
; We need to change the right-branch, left-branch and branch-structure
; that is the element (branches and structures) lookup procedures.
; balanced? and total-weight are abstracted around the lookup
;procedures so there
; is no need to change them.
(define (make-mobile-cons left right)
  (cons left right))

(define (make-branch-cons length structure)
  (cons length structure))

(define (right-branch-cons mobile)
  (cdr mobile))

(define (left-branch-cons mobile)
  (car mobile))

(define (branch-length-cons branch)
  (car branch))

(define (branch-structure-cons branch)
  (cdr branch))

(define (total-weight-cons mobile)
  (cond ((null? mobile) 0)
	((not (pair? mobile)) mobile)
	(else
	 (+ (total-weight-cons (branch-structure-cons (right-branch-cons mobile)))
	    (total-weight-cons (branch-structure-cons (left-branch-cons mobile)))))))


(define (balanced?-cons mobile)
  (cond ((null? mobile) #t)
	((not (pair? mobile)) #t)
	(else
	 (and
	  (=
	   (* (branch-length-cons (right-branch-cons mobile))
	      (total-weight-cons (branch-structure-cons (right-branch-cons mobile))))
	   (* (branch-length-cons (left-branch-cons mobile))
	      (total-weight-cons (branch-structure-cons (left-branch-cons mobile)))))
	  (balanced?-cons (branch-structure-cons (right-branch-cons mobile)))
	  (balanced?-cons (branch-structure-cons (left-branch-cons mobile)))))))

(balanced?-cons (make-mobile-cons (make-branch-cons 1 (make-mobile-cons (make-branch-cons 3 2) (make-branch-cons 4 82))) (make-branch-cons 12 21)))
(balanced?-cons (make-mobile-cons (make-branch-cons 1 (make-mobile-cons (make-branch-cons 1 2) (make-branch-cons 1 2))) (make-branch-cons 2 2)))
