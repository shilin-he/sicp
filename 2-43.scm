(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) 
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    (list)
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) 
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate + 
              0 
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (pairs n)
  (accumulate append 
              (list)
              (map (lambda (i) 
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? 
               (unique-pairs n))))


(define (prime? x)
  (define (iter test)
    (cond ((> (* test test) x) #t)
          ((= (remainder x test) 0) #f)
          (else (iter (+ test 1)))))
  (iter 2))

(define (permutation s)
  (if (null? s)
    (list(list))
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutation (remove x s))))
             s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list x y))
                  (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))


(define (sum-of-distinct-ordered-triples n sum)
  (filter (lambda (t) (= (+ (car t) (cadr t) (caddr t)) sum))
          (flatmap 
            (lambda (x)
              (flatmap (lambda (y)
                         (map (lambda (z) (list x y z))
                              (enumerate-interval 1 (- y 1))))
                       (enumerate-interval 1 (- x 1))))
            (enumerate-interval 1 n))))

(define (queen board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter 
        (lambda (positions) (safe? k positions))
        (flatmap 
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board (list))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (safe? k positions)
  (define (test i k)
    (cond ((>= i k) #t)
          ((let ((i-pos (list-ref positions i))
                 (k-pos (list-ref positions k)))
             (or (= i-pos k-pos)
                 (and (= (+ i 1) i-pos)
                      (= (+ k 1) k-pos))))
           #f)
          (else (test (+ i 1) k))))
  (test 0 (- k 1)))


; How long will it take? (* (expt board-size k) T)
