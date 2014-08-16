; (define (sum term a next b)
;   (if (> a b)
;     0
;     (+ (term a)
;        (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

; (define (product term a next b)
;   (if (> a b)
;     1
;     (* (term a) 
;        (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))


(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (pi-term x)
    (/ (* 4 x x)
       (* (- (* 2 x) 1) (+ (* 2 x) 1))))
  (* 2.0
     (product pi-term 1 inc n)))
