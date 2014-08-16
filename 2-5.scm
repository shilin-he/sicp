(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

; (define (div-num x y)
;   (define (iter x result)
;     (if (= (remainder x y) 0)
;       (iter (/ x y) (+ result 1))
;       result))
;   (iter x 0))

(define (div-num x y)
  (if (= (remainder x y) 0)
    (+ 1 (div-num (/ x y) y))
    0))

(define (car x) (div-num x 2))

(define (cdr x) (div-num x 3))
