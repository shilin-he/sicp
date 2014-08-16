(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment x y)
  (cons x y))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point (/ (+ (car (start-segment s))
                    (car (end-segment s)))
                 2.0)
              (/ (+ (cdr (start-segment s))
                    (cdr (end-segment s)))
                 2.0)))


; (define (make-rect a b c d)
;   (cons (cons a b) (cons c d)))

; (define (rect-width r)
;   (let ((a (car (car r)))
;         (b (cdr (car r))))
;     (abs (- (car a) (car b)))))

; (define (rect-height r)
;   (let ((a (car (car r)))
;         (c (car (cdr r))))
;     (abs (- (cdr a) (cdr c)))))

(define (make-rect x y)
  (cons x y))

(define (rect-width r)
  (let ((x (car r))
        (y (cdr r)))
    (abs (- (car x)
            (car y)))))

(define (rect-height r)
  (let ((x (car r))
        (y (cdr r)))
    (abs (- (cdr x)
            (cdr y)))))

(define (rect-perimeter r)
  (* 2 (+ (rect-width r)
          (rect-height r))))

(define (rect-area r)
  (* (rect-width r)
     (rect-height r)))

