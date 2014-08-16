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
        ((even? n) (expt-fast (square b) (/ n 2)))
        (else (* b (expt-fast b (- n 1))))))

(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (expt-fast-iter b n)
  (define (iter counter product extra) 
    (cond ((= counter 0) 1)
          ((= counter 1) (* product extra))
          ((even? counter) (iter (/ counter 2) (square product) extra))
          (else (iter (/ (- counter 1) 2) 
                      (square product) 
                      (* extra product)))))
  (iter n b 1))

