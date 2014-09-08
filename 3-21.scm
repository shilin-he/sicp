(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue value)
  (let ((item (cons value '())))
    (if (empty-queue? queue)
      (begin (set-front-ptr! queue item)
             (set-rear-ptr! queue item)
             queue)
      (begin (set-cdr! (rear-ptr queue) item)
             (set-rear-ptr! queue item)
             queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else 
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

(define (print-queue queue)
  (front-ptr queue))
