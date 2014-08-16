(define (square-sum x y) (+ (* x x) (* y y)))
(define (>= x y) (not (< x y)))

(define (square-sum-of-larger-two x y z)
  (cond ((and (>= x z) (>= y z)) (square-sum x y))
        ((and (>= x y) (>= z y)) (square-sum x z))
        ((and (>= y x) (>= z x)) (square-sum y z))))
