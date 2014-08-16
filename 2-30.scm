; (define (square-tree tree)
;   (map (lambda (x)
;          (if (pair? x)
;            (square-tree x)
;            (* x x)))
;        tree))

(define (square-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))


