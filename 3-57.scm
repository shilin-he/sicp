(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else 
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((> s1car s2car)
                   (cons-stream 
                     s1car 
                     (merg (stream-cdr s1) s2)))
                  ((> s2car s1car)
                   (cons-stream 
                     s2car 
                     (merg (stream-cdr s2) s1)))
                  (else
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1)
                             (stream-cdr s2)))))))))

(define S 
  (cons-stream 
    1 
    (merge (merge S (scale-stream S 2))
           (merge (scale-stream S 3) (scale-stream S 5)))))
