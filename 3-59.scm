(define (integrate-series s)
  (stream-map / s integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (- x))
                             (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
