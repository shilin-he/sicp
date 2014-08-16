(define (last-pair items)
  (cond ((null? items) (list))
        ((= (length items) 1) (list (car items)))
        (else (last-pair (cdr items)))))

