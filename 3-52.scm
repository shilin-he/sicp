(define the-empty-stream '())

(define (stream-empty? s) (null? s))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-empty? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-empty? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream 
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-empty? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
           (stream-car stream)
           (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (show x)
  (display-line x)
  x)

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; (define y (stream-filter even? seq))
; (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (add-stream s1 s2) (stream-map + s1 s2))

(define (partial-sum stream)
  (let ((partial 
          (cons-stream 
            (stream-car stream)
            (add-stream (stream-cdr stream)
                        partial))))
    partial))

