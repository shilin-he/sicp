(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (split op1 op2)
  (define (iter painter n)
    (if (= n 0)
      painter
      (let ((smaller (iter painter (- n 1))))
        (op1 painter (op2 smaller smaller)))))
  iter)

(define right-split (split beside below))

(define up-split (spit below beside))
