(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) 
           (/ trials-passed trials))
          ((experiment) 
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else 
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random-real) range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (monte-carlo trials (integral-test p x1 x2 y1 y2)))

(define (square x) (* x x))

(define (integral-test p x1 x2 y1 y2)
  (let ((low-x (if (> x1 x2) x2 x1))
        (high-x (if (> x1 x2) x1 x2))
        (low-y (if (> y1 y2) y2 y1))
        (high-y (if (> y1 y2) y1 y2)))
    (lambda () 
      (p (random-in-range low-x high-x)
         (random-in-range low-y high-y)))))

(define (in-the-circle-test center-x center-y radius)
  (lambda (x y)
    (<= (+ (square (- x center-x))
           (square (- y center-y)))
        (square radius))))

(define (estimate-pi trials)
  (* 4.0 
     (estimate-integral 
       (in-the-circle-test 0 0 1) -1 1 -1 1 trials)))
