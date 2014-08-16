;; a. Use data directed programming to dispatch different 
;; derivative operations according to the operators.
;; number? and same-variable? don't have operators.
;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) 
               (operands exp)
               var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (variable? e) (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) 
  (cond ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list '+ a1 a2))))

(define (make-product m1 m2) 
  (cond ((or (and (number? m1) (= m1 0))
             (and (number? m2) (= m2 0))) 0)
        ((and (number? m1) (= m1 1) m2))
        ((and (number? m2) (= m2 1) m1))
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        (else (list '** base exponent))))

(define (install-derivative-of-sum)
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))
  (put 'deriv '+ deriv-sum)
  'Done)

(define (install-derivative-of-product)
  (define (deriv-product operands var)
    (make-sum
      (make-product (car operands)
                    (deriv (cadr operands) var))
      (make-product (cadr operands)
                    (deriv (car operands) var))))
  (put 'deriv '* deriv-product)
  'Done)

(define (install-derivative-of-expt)
  (define (deriv-expt operants var)
    (make-product (make-exponentiation 
                    (car operands) 
                    (- (cadr operands) 1))
                  (make-product (cadr operands)
                                (deriv (car operands) var))))
  (put 'deriv '** deriv-expt)
  'Done)

; d. All the put operations need to be changed.
;
