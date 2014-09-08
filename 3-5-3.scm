(define (sqrt-stream x)
  (define guesses
    (cons-stream 
      1.0
      (stream-map 
        (lambda (guess) (sqrt-improve guess x))
        guesses)))
  guesses)


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define (pi-stream)
  (scale-stream 
    (partial-sum (pi-summands 1)) 
    4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))

(define (accelarated-sequence transform s)
  (stream-map stream-car 
              (make-tableau transform s)))

(display-stream (accelerated-sequence euler-transform
                                      pi-stream))
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
      (stream-car s1)
      (interleave s2 (stream-cdr s1)))))
