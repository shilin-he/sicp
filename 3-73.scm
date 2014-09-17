(define (integral integrand initial-value dt)
  (define int
    (cons-stream 
      initial-value 
      (add-stream (scale-stream integrand dt) int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-stream (integral (scale-stream i (/ 1 C)) v0 dt)
                (scale-stream i R))))
