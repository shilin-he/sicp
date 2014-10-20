(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame) (map car frame))

(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (> (length vars) (length vals))
      (error "Too few arguments supplied" vars vals)
      (error "Too many arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
            ((eq? (caar frame) var) (cdar frame))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame) (env-loop (enclosing-environment env)))
            ((eq? (caar frame) var) (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame) (add-binding-to-frame! var val frame))
            ((eq? (caar frame) var) (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (scan frame)))

