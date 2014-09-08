(define (partial-sum stream)
  (let ((partial 
          (cons-stream 
            (stream-car stream)
            (add-stream (stream-cdr stream)
                        partial))))
    partial))
