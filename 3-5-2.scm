(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fib (fibgen 0 1))

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream 
    (stream-car stream)
    (sieve (stream-filter 
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))

(define ones (cons-stream 1 ones))

(define (add-stream s1 s2) (stream-map + s1 s2))

(define integers (cons-stream 1 (add-stream integers ones)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-stream (stream-cdr fibs)
                                        fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double)))

(define primes
  (cons-stream 
    2
    (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (sqaure (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))
