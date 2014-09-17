(define (make-zero-crossings input-stream last-value average)
  (let ((avpt (average (stream-car input-stream) last-value)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt
                                      average))))
