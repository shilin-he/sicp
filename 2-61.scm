(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((< x (car set) #f))
        ((= x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) 
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    (list)
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((< x1 x2) (intersection-set (cdr set1) set2))
            ((= x1 x2) (cons x1 (intersection-set
                                  (cdr set1) (cdr set2))))
            (else (intersection-set set1 (cdr set2)))))))

(define (union set1 set2) 
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((< x1 x2) 
                   (cons x1 (union (cdr set1) set2)))
                  ((= x1 x2) 
                   (union (cdr set1) (cdr set2)))
                  (else 
                    (cons x2 (union set1 (cdr set2)))))))))
