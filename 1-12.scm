(define (pascal-triangle row col)
  (if (or (= col 1) (= col row))
    1
    (+ (pascal-triangle (- row 1) col)
       (pascal-triangle (- row 1) (- col 1)))))
