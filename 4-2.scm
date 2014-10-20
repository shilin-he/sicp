;; When Louis's evaluator evaluates (define x 3), x will be treated as a 
;; variable and lookup-variable-value procedure will try to get the value
;; of x, but at the monent x hasn't been defined. So it will cause unbound
;; variable error. 

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) 
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (application? exp) (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))
