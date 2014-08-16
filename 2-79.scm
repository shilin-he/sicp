(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sum x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC"
               (list op type-tags))))))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag x))
  (put 'add '(scheme-number scheme-number) 
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) 
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) 
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) 
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) 
       (lambda (x y) (= x y)))
  (put 'make 'scheme-number (lambda (x) (tag (x))))
  'done)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

(define (install-rational-package)
  ;; internal procedures
  (define (make-rat n d) 
    (let ((g (abs (gcd n d)))
          (n (if (> (* n d) 0) (abs n) (- (abs n))))
          (d (abs d))) 
      (cons (/ n g) (/ d g))))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) 
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) 
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat  (* (numer x) (numer y))
               (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  ;; interface to the rest of system
  (define (tag x) (attach-tag x))
  (put 'add '(rational rational) 
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) 
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) 
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) 
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (magnitude z1) (magnitude z2))
         (= (angle z1) (angle z2))))
  ;; interface to the rest of system
  (define (tag x) (attach-tag x))
  (put 'add '(complex complex) 
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex) 
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex) 
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex) 
       (lambda (x y) (tag (div-complex x y))))
  (put 'equ? '(complex complex) equ?)
  (put 'make-from-real-img 'complex 
       (lambda (x y) (tag (make-complex-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex 
       (lambda (r a) (tag (make-complex-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-real-imag a r)
  ((get 'make-from-mag-ang 'complex) a r))
