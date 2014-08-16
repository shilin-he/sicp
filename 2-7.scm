(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (min a b)))

(define (upper-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (max a b)))
