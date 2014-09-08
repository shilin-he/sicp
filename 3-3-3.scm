(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record 
      (cdr record)
      #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

; (define (make-table) (list '*table*))

(define (lookup key1 key2 table)
  (let ((sub-table (assoc key1 (cdr table))))
    (if sub-table
      (let ((record (assoc key2 (cdr sub-table))))
        (if record
          (cdr record)
          #f))
      #f)))

(define (insert! key1 key2 value table)
  (let ((sub-table (assoc key1 (cdr table))))
    (if sub-table
      (let ((record (assoc key2 (cdr sub-table))))
        (if record
          (set-cdr! record value)
          (set-cdr! sub-table 
                    (cons (cons key2 value) (cdr sub-table)))))
      (set-cdr! table
                (cons (list key1 (cons key2 value))
                      (cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1 (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
