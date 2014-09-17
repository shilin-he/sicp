(define (smooth stream)
  (stream-map (lambda (x y) (/ (+ x y) 2))
              stream
              (stream-cdr stream)))

(define (make-zero-crossings input-stream last-value)
  (let ((smoothed-stream (smooth input-stream)))
    (cons-stream (sign-change-detector 
                   (stream-car smoothed-stream)
                   last-value)
                 (make-zero-crossings 
                   (stream-cdr smoothed-stream)
                   (stream-car smoothed-stream)))))
