(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (min a b)))

(define (upper-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (max a b)))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


