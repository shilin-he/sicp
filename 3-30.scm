(define (ripple-carry-adder a b s c-in)
  (define (iter a b s c-in)
    (cond ((null? a) 'ok)
          (else
            (let ((c-out (make-wire)))
              (full-adder (car a) (car b) c-in (car s) c-out)
              (iter (cdr a) (cdr b) (cdr s) c-out)))))
  (iter a b s c-in))

