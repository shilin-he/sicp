(define (make-monitored f)
  (let ((count 0))
    (lambda (m)
      (cond ((eq? 'how-many-calls? m) count)
            ((eq? 'reset-count m) (set! count 0))
            (else (begin (set! count (+ count 1))
                         (f m)))))))

