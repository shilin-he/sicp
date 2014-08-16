

(define (identity x) x)

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum-integers a b)
  (sum identity a inc b))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

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

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (add-2-h x) (+ x (* 2 h)))
  (* (/ h 3.0)
     (+ (f a)
        (* 2 (sum f (+ a (* 2 h)) add-2-h b))
        (* 4 (sum f (+ a h) add-2-h b))
        (f b))))
