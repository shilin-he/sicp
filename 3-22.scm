(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (insert-queue! value)
      (let ((item (cons value '())))
        (cond ((null? front-ptr) 
               (set-front-ptr! item)
               (set-rear-ptr! item)
               dispatch)
              (else
                (set-cdr! rear-ptr item)
                (set-rear-ptr! item)
                dispatch))))
    (define (delete-queue!)
      (cond ((null? front-ptr)
             (error 
               "DELETE! called with an empty queue" queue))
            (else 
              (set-front-ptr! (cdr front-ptr))
              dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'empty-queue?) (null? front-ptr))
            ((eq? m 'front-queue) 
             (if (null? front-ptr)
               (error "FRONT called with an empty queue" queue)
               (car front-ptr)))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))))
    dispatch))

(define (front-ptr queue) (queue 'front-ptr))

(define (rear-ptr queue) (queue 'rear-ptr))

(define (set-front-ptr! queue item) 
  ((queue 'set-front-ptr!) item))

(define (set-rear-ptr! queue item) 
  ((queue 'set-rear-ptr!) item))

(define (empty-queue? queue) (queue 'empty-queue?))

(define (front-queue queue)
  (queue 'front-queue))

(define (insert-queue! queue value)
  ((queue 'insert-queue!) value))

(define (delete-queue! queue)
  (queue 'delete-queue!))

(define (print-queue queue)
  (front-ptr queue))
