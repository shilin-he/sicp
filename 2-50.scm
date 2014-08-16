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

(define (frame-outline->painter frame)
  ((segments->painter (list (make-segment (make-vect 0 0)
                                          (make-vect 0 1))
                            (make-segment (make-vect 0 1)
                                          (make-vect 1 1))
                            (make-segment (make-vect 1 1)
                                          (make-vect 1 0))
                            (make-segment (make-vect 1 0)
                                          (make-vect 0 0))))
   frame))

(define (frame-cross->painter frame)
  ((segments->painter (list (make-segment (make-vect 0 0)
                                          (make-vect 1 1))
                            (make-segment (make-vect 1 0)
                                          (make-vect 0 1))))
   frame))

(define (frame-diamond->painter frame)
  ((segments->painter (list (make-segment (make-vect 0 0.5)
                                          (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1)
                                          (make-vect 1 0.5))
                            (make-segment (make-vect 1 0.5)
                                          (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1)
                                          (make-vect 0 0.5))))
   frame))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter 
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left 
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

; ?????????????
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

; ?????????????
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))



