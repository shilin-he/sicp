(define (f-recursive n)
  (if (< n 3) n
    (+ (f-recursive (- n 1)) 
       (* 2 (f-recursive (- n 2))) 
       (* 3 (f-recursive (- n 3))))))

(define (f-iter n)
  (define (iter a b c count)
    (if (> count (- n 2))
      c
      (iter b 
            c 
            (+ c 
               (* 2 b) 
               (* 3 a)) 
            (+ count 1))))
  (if (< n 3) n
    (iter 0 1 2 1)))
