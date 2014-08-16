;; constructors, selectors, predicates
(define (variable? e) (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? e) (and (pair? e) (eq? (car e) '+)))
(define (addend e) (cadr e))
(define (augend e) (caddr e))
(define (make-sum a1 a2) 
  (cond ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list '+ a1 a2))))
(define (product? e) (and (pair? e)(eq? (car e) '*)))
(define (multiplier e) (cadr e))
(define (multiplicand e) (caddr e))
(define (make-product m1 m2) 
  (cond ((or (and (number? m1) (= m1 0))
             (and (number? m2) (= m2 0))) 0)
        ((and (number? m1) (= m1 1) m2))
        ((and (number? m2) (= m2 1) m1))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
           (make-product (multiplier exp) 
                         (deriv (multiplicand exp) var))
           (make-product (multiplicand exp) 
                         (deriv (multiplier exp) var))))
        (else (error "unkown express type -- DERIV" exp))))


