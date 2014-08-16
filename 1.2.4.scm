(define (expt-recur b n)
  (if (= n 0)
    1
    (* b (expt-recur b (- n 1)))))

(define (expt-iter b n)
  (define (iter counter product)
    (if (= counter 0)
      product
      (iter (- counter 1) (* b product))))
  (iter n 1))

(define (expt-fast b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-fast b (/ n 2))))
        (else (* b (expt-fast b (- n 1))))))

(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (expt-fast-iter b n)
  (define (iter count result) 
    (if (< count 1)
      result
      (iter (/ count 2) (* result result))))
  (cond ((= n 0) 1)
        ((even? n) (iter (/ n 2) b))
        (else (* b (iter (/ (- n 1) 2) b)))))

