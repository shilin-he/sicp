(define (make-semaphore n)
  (let ((cell (list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-inc! cell n)
               (the-semaphore 'acquire)))
            ((eq? m 'release) (dec! cell n))))
    the-semaphore))

(define (dec! cell n)
  (if (> (car cell) (- n 1))
    (error "Invalid operation -- INC!" cell n)
    (set-car! cell (- (car cell) 1))))

(define (test-and-inc! cell n)
  (if (>= (car cell) n)
    #t
    (begin (set-car! cell (+ (car cell) 1))
           #f)))
