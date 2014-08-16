(define (reverse items)
  (if (<= (length items) 1)
    items
    (append (reverse (cdr items)) (list (car items)))))

