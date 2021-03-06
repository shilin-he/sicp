(define (install-integer-package)
  (define (raise x) ((get 'make 'rational) (contents x) 1))
  (put 'raise 'integer raise))

(define (install-rational-package)
  (define (raise x) ((get 'make 'real) 
                     (* (/ (denom x) (numer x)) 1.0)))
  (define (project x) ((get 'make 'integer) (denom x)))
  (put 'raise 'rational raise)
  (put 'project 'rational project))

(define (install-real-package)
  (define (raise x) ((get 'make-from-real-imag 'complex) 
                     (contents x) 0))
  (define (project x) ((get 'make 'rational) 
                       (round (contents x)) 1))
  (put 'raise 'real raise)
  (put 'project 'real project))

(define (install-complex-package)
  (define (project x) ((get 'make 'real) 
                       (real-part (contents x))))
  (put 'project 'complex project))

(define (raise x) ((get 'raise (type-tag x)) x))
(define (project x) ((get 'project (type-tag x)) x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((op (get op type-tags)))
      (if op
        (drop (apply op (map contents args)))
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

(define (drop x)
  (if (equ? (raise (project x)) x)
    (drop (project x))
    x))
