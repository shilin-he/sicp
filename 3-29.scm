(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c2 c1 c-out)))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay 
        inverter-delay 
        (lambda () (set-signal! ouput new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 1) 0)
        ((= s 0) 1)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value 
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay 
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a1 a2)
  (if (and (= a1 1) (= a2 1)) 1 0))

(define (or-gate a b output)
  (let ((i1 (make-wire))
        (i2 (make-wire))
        (a1 (make-wire)))
    (inverter a i1)
    (inverter b i2)
    (and-gate i1 i2 a1)
    (inverter a1 output))
