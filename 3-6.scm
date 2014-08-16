(define (rand-update x) (+ x 1))

(define rand
  (let ((x 0))
    (lambda (message)
      (cond ((eq? message 'generate) 
             (begin (set! x (rand-update x)) x))
            ((eq? message 'reset) 
             (lambda (new-value) (set! x new-value)))
            (else (error "Unknown symbol -- RAND" message))))))
