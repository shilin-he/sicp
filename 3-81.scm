(define (generate-random-number initial-value requests)
  (cons-stream
    initial-value
    (cond ((eq? 'generate (stream-car requests))
           (generate-random-number 
             (random-update initial-value)
             (stream-cdr requests)))
          ((number? (stream-car requests))
           (generate-random-number
             (stream-car requests))
           (stream-cdr requests))
          (else (error "Wrong requst -- " 
                       (stream-car requests))))))

