(define (install-integer-package)
  (define (raise x) ((get 'make 'rational) (contents x) 1))
  (put 'raise 'integer raise))

(define (install-rational-package)
  (define (raise x) ((get 'make 'real) 
                     (* (/ (denom x) (numer x)) 1.0)))
  (put 'raise 'rational raise))

(define (install-real-package)
  (define (raise x) ((get 'make-from-real-imag 'complex) 
                     (contents x) 0))
  (put 'raise 'real raise))

(define (raise x) ((get 'raise (type-tag x)) x))
