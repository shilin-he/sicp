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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((op (get op type-tags)))
      (if op
        (apply op (map contents args))
        (if (and (= (length args) 2) 
                 (not (eq? (car type-tags) (cadr type-tags))))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (higher type1 type2)
              (apply-generic arg1 (raise arg2))
              (apply-generic (raise arg1) arg2)))
          (else "No method for these types"
                (list op type-tags)))))))


(define (higher type1 type2)
  (define tower (list (cons 'integer 1)
                      (cons 'rational 2)
                      (cons 'real 3)
                      (cons 'complex 4)))
  (define (level type tower)
    (cond ((null? tower) (error "Unknown type -- HIGHER" type))
          ((eq? (car (car tower)) type) (cdr (car tower)))
          (else (level type (cdr tower)))))
  (> (level type1 tower) (level type2 tower)))

(define (equ? x y) (apply-generic 'equ? x y))
