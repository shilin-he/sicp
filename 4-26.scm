(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (unless->if exp)
  (make-if (cadr exp) (caddr exp) (cadddr exp)))

; Implement unless in applicative order as a special form, the evaluation of
; arguments will be delayed. Since it's a special form, not a procedure, it
; can not be used as argument and return value of procedures.
