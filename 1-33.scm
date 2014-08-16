(define (sum-squares-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-coprimes a b)
  (define (coprime-to-b i)
    (= (gcd i b) 1))
  (filtered-accumulate * 1 identity a inc b coprime-to-b))

(define (filtered-accumulate combiner null-value term a next b filter?)
  (if (> a b)
    null-value
    (combiner (if (filter? a)
                (term a)
                null-value)
              (filtered-accumulate 
                combiner null-value term (next a) next b filter?))))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (even? x) (= (modulo x 2) 0))

(define (odd? x) (= (modulo x 2) 1))

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

