(define (make-withdraw initial-amount)
  ((lambda (balance) 
     (lambda (amount)
       (if (>= balance amount)
         (begin (set! (balance (- balance amount)))
                balance)
         (error "Insufficient funds"))))
   initial-amount))
