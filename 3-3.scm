(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define times-of-wrong-password 0)
  (define (call-the-cops) (dispaly "Called the cops..."))
  (define (dispatch pw m)
    (if (eq? pw password)
      (begin (set! times-of-wrong-password 0)
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else 
                     (error "Unknown request -- MAKE-ACCOUNT"
                            m))))
      (begin (set! 
               times-of-wrong-password 
               (+ times-of-wrong-password 1))
             (if (> times-of-wrong-password 7)
               (call-the-cops)
               (error "Incorrect password")))))
  dispatch)