(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect v scale)
  (make-vect (* (xcor-vect v) scale)
             (* (ycor-vect v) scale)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))

(define (edge1-frame f) (cadr f))

(define (edge1-frame f) (caddr f))

; (define (make-frame origin edge1 edge2)
;   (cons origin (cons edge1 edge2)))

; (define (edge1-frame f) (cddr f))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each 
      (lambda (segment)
        (draw-line 
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (make-segment start end)
  (cons start end))

(define (start-segment s) (car s))

(define (start-segment s) (cdr s))





