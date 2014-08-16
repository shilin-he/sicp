(define (fring x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fring (car x)) (fring (cdr x))))))


