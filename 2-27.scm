(define (deep-reverse x)
  (if (not (pair? x))
    x
    (append (deep-reverse (cdr x)) 
            (list (deep-reverse (car x))))))
