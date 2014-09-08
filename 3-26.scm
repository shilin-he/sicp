(define (assoc keys records)
  (cond ((null? records) #f)
        ((equal? keys (caar records)) (car records))
        (else (assoc keys (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((record (assoc keys (cdr local-table))))
        (if record 
          (cdr record)
          #f)))
    (define (lookup given-key set-of-records)
      (if (null? set-of-records)
        #f
        (let ((entry-key (key (entry set-of-records))))
          (cond ((= given-key entry-key) 
                 (entry set-of-records))
                ((< given-key entry-key) 
                 (lookup given-key 
                         (left-branch set-of-records)))
                ((> given-key entry-key)
                 (lookup given-key 
                         (right-branch set-of-records)))))))
    (define (insert! keys value)
      (let ((record (assoc keys (cdr local-table))))
        (if record
          (set-cdr! record value)
          (set-cdr! local-table 
                    (cons (cons keys value) 
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
    #f
    (let ((entry-key (key (entry set-of-records))))
      (cond ((= given-key entry-key) (entry set-of-records))
            ((< given-key entry-key) 
             (lookup given-key (left-branch set-of-records)))
            ((> given-key entry-key)
             (lookup given-key 
                     (right-branch set-of-records)))))))
