(define (* a b)
  (if (= b 0)
    0
    (+ a (* a (- b 1)))))


(define (fast-multiply a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-multiply (double a) (halve b)))
        (else (+ a (fast-multiply (double a) (halve (- b 1)))))))

(define (double x) (+ x x))

(define (halve x) (/ x 2))


(define (fast-multiply-iter a b)
  (define (iter counter product extra)
    (cond ((= counter 0) 0)
          ((= counter 1) (+ product extra))
          ((even? counter) (iter (halve counter) (double product) extra))
          (else (iter (halve (- counter 1)) 
                      (double product) 
                      (+ extra product)))))
  (iter b a 0))
