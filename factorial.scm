(define (factorial-recursive x)
  (if (= x 1) 1 
    (* x (factorial (- x 1)))))

(define (factorial-iter n) 
  (define (fac-iter product counter) 
    (if (> counter n) product
      (fac-iter (* product counter)
                (+ counter 1))))
  (fac-iter 1 1))
