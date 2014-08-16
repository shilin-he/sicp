(define (cube-root-iter guess x)
  (if (good-enough? guess x) 
    guess
    (cube-root-iter (improve guess x) x)))

(define (improve guess x)
  (average guess 
           (/ (+ (/ x (* guess guess)) (* 2 guess)) 3.0)))

(define (average x y) (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) (/ guess 100.0)))

(define (cube-root x)
  (cube-root-iter 1.0 x))
