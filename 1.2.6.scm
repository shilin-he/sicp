(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2) 
    3
    (+ n 2)))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) 
                                m))
        (else (remainder (* base (expmod base (- exp 1) m)) 
                         m))))
  ; (remainder (expt-fast base exp) m))

(define (expt-fast b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-fast b (/ n 2))))
        (else (* b (expt-fast b (- n 1))))))

(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ (random-integer (- n 1)) 1)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (carmichael-numer-test n)
  (define (test a)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (test (+ a 1)))
          (else #f)))
  (test 2))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (cpu-time)))

(define (start-prime-test n start-time)
  (if (fast-prime? n (log n))
    (report-prime (- (cpu-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n times)
  (define (search n times)
    (timed-prime-test n)
    (if (> (- times 1) 0)
      (search (+ n 2) (- times 1))))
  (if (= (remainder n 2) 0)
    (search (+ n 1) times)
    (search n times))
  (newline))





