(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch x)
  (car x))

(define (right-branch x)
  (car (cdr x)))

(define (branch-length x)
  (car x))

(define (branch-structure x)
  (car (cdr x)))

(define (total-weight x)
  (if (number? x)
    x
    (+ (total-weight (branch-structure (left-branch x)))
       (total-weight (branch-structure (right-branch x))))))

(define (balanced? x)
  (if (number? x)
    #t
    (let ((lb (left-branch x))
          (rb (right-branch x)))
      (and (= (* (branch-length lb) 
                 (total-weight (branch-structure lb)))
              (* (branch-length rb)
                 (total-weight (branch-structure rb))))
           (balanced? (branch-structure lb))
           (balanced? (branch-structure rb))))))

