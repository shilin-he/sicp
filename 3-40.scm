(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;; p2 sets x to 1000, then p1 sets x to 100 => 100
;; p1 sets x to 100, then p2 sets x to 1000 => 1000
;; p1 accesses x, then p2 sets x to 1000, then p1 sets x => 10,000
;; p2 accesses x twice, then p1 sets x to 100, the p2 sets x => 10,000
;; p2 accesses x, then p1 sets x to 100, then p2 accesses x twice, the p2 sets x => 100,000
;; p1 sets x to 100, then p2 accesses x three times, then p2 sets x => 1,000,000
;; p2 sets x to 1000, then p1 accesses x three times, then p1 sets x => 1,000,000
;;

(define x 10)

(define s (make-serializer))

(parallel-execute 
  (s (lambda () (set! x (* x x))))
  (s (lambda () (set! x (* x x x)))))
;; p1 sets x to 100, p2 sets x to 1,000,000
;; p2 setx to 1000, p1 sets x to 1,000,000
