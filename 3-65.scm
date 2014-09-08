(define (ln2-stream n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-stream (+ n 1)))))

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
                                      ln2-stream))
