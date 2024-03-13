; record-files [ employee-name -> employee-record ]
; employee-record [ address , salary ... ]
(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

(define (get-record filename employee-number)
  (let ((op (get 'get-record filename)))
    (if op
	(op employee-number)
	(error "bad file: GET-RECORD" filename))))
(define (get-salary filename employee-number)
  (let ((op (get 'get-salary filename)))
    (if op
	(if (get-record filename employee-number)
	    (op (get-record filename employee-number))
	    (error "employee not found: GET-SALARY" employee-number))
	(error "bad file: GET-SALARY" filename))))
(define (find-employee employee-number files)
  (if (null? files)
      '()
      (let ((rec (get-record (car  files)
			     employee-number)))
	(if rec
	    rec
	    (find-employee employee-number (cdr files))))))

(define (install-records-package)
  ; company a
  (define company-a-records
	   (make-hash-table))
  (define (make-employee-record-a name salary address)
    (list name salary address))
  (define (put-record-a
	   company-file
	   employee-number
	   employee-name
	   address
	   salary)
    (hash-table/put! company-file
		     (list employee-number)
			   (make-employee-record-a
			    employee-name
			    address
			    salary)))
  (put-record-a company-a-records
		12345
		"Jhon"
		"300 Kingston"
		444)
  (put-record-a company-a-records
		126621
		"Sindy"
		"1050 Charleston"
		1000)
  (define (get-record-a employee-number)
    (let ((rec (hash-table/get
		company-a-records
		(list employee-number)
		#f)))
      rec))
  (define (get-salary-a employee-record)
	  (caddr employee-record))
      

  ; company b
  (define company-b-records
	   (make-hash-table))
  (define (make-employee-record-b name salary address)
    (list name salary address))
  (define (put-record-b
	   company-file
	   employee-number
	   employee-name
	   address
	   salary)
    (hash-table/put! company-file
		     (list employee-number)
			   (make-employee-record-b
			    employee-name
			    address
			    salary)))
  (put-record-b company-b-records
		1234
		"Luarel"
		"200 Lindson"
		446)
  (define (get-record-b employee-number)
    (let ((rec
	   (hash-table/get company-b-records
			    (list employee-number)
			    #f)))
      rec))
  (define (get-salary-b record)
	  (caddr record))

  (put 'get-record 'company-a get-record-a)
  (put 'get-record 'company-b get-record-b)
  (put 'get-salary 'company-a get-salary-a)
  (put 'get-salary 'company-b get-salary-b)
  'done)

(install-records-package)

(get-record 'company-a 12345)
(get-record 'company-b 1234)
(get-salary 'company-a 126621)
(get-salary 'company-b 1234)
(find-employee 12345 '(company-a company-b))
(find-employee 1234 '(company-a company-b))
(find-employee 1 '(company-a company-b))
