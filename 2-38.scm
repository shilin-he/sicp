(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) 
        (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))


(/ 1 (/ 2 (/ 3 1)))
(/ (/ (/ 1 1) 2) 3)

(1 (2 (3 ())))
(((() 1) 2) 3)

; fold-right and fold-left will product the save value if
; the operator is commutative (if change the order of the 
; operants does not change the result).

