(define (make-personnel-file division file) 
  (cons division file))

(define (make-employee division employee) 
  (cons division employee))

(define (division datum)
  (if (pair? datum)
    (car datum)
    (error "Bad division datum -- DIVISION" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad division datum -- CONTENTS" datum)))

(define (get-record employee-name personnel-file)
  ((get 'get-record (division personnel-file)) 
   employee-name (contents personnel-file)))

(define (get-salary employee)
  ((get 'get-salary (division employee)) (contents employee)))

(define (find-employee-record employee-name personnel-files)
  (if (null? personnel-files) 
    (error "The specified employee not found --" employee-name)
    (let ((employee 
            (get-record employee-name (car personnel-files))))
      (if employee 
        employee 
        (find-employee-record 
          employee-name 
          (cdr personnel-files))))))

(define (install-new-division)
  (define (get-record employee-name personnel-file))
  (define (get-salary employee))
  (put 'get-record 'new-division get-record)
  (put 'get-salary 'new-division get-salary)
  'Done)




