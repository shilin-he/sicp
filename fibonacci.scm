(define (fibonocci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonocci (- n 1))
                 (fibonocci (- n 2))))))

(define (fib-iter n)
  (define (iter a b count)
    (if (= count 0)
      b
      (iter (+ a b) a (- count 1))))
  (iter 1 0 n))
