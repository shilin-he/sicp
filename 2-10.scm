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


(define (div-interval x y)
  (let ((lower-y (lower-bound y))
        (upper-y (upper-bound y)))
    (if (<= (* lower-y upper-y) 0)
      (error "The divisor spans 0" y)
      (make-interval x (make-interval (/ 1.0 upper-y)
                                      (/ 1.0 lower-y))))))
