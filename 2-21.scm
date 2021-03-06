(define (square x) (* x x))

(define (square-list-1 items)
  (if (null? items)
    (list)
    (cons (square (car items))
          (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))
