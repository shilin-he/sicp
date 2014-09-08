;; except 110
;;
;;
(define x 10)

(define s (make-serializer))

(parallel-execute 
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))
