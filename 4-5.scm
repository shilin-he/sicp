(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE is not last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (if (tagged-list? (cond-actions first) '=>)
                   (list (cadr (cond-actions first)) 
                         (cond-predicate first))
                   (sequence->exp (cond-actions first)))
                 (expand-clauses rest))))))


