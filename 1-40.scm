(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damping f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damping 
                 (lambda (y) (/ x y)))
               1.0))

(define (square x) (* x x))

(define (cube-root x)
  (fixed-point (average-damping
                 (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; (define (sqrt x)
;   (newtons-method (lambda (y) (- (square y) x))
;                   1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (newtons-method
    (lambda (y) (- (square y) x))
    1.0))

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))

