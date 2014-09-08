(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))

(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque deque)
    (error "FRONT called with an empty deque" deque)
    (car (front-ptr deque))))

(define (front-insert-deque! deque value)
  (let ((item (cons value (cons '() '()))))
    (if (empty-deque? deque)
      (begin (set-front-ptr! deque item)
             (set-rear-ptr! deque item)
             deque)
      (begin (set-car! (cdr (front-ptr deque)) item)
             (set-cdr! (cdr item) (front-ptr deque))
             (set-front-ptr! deque item)
             deque))))

(define (rear-insert-deque! deque value)
  (let ((item (cons value (cons '() '()))))
    (if (empty-deque? deque)
      (begin (set-front-ptr! deque item)
             (set-rear-ptr! deque item)
             deque)
      (begin (set-cdr! (cdr (rear-ptr deque)) item)
             (set-car! (cdr item) (rear-ptr deque))
             (set-rear-ptr! deque item)
             deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else 
          (set-front-ptr! deque (cddr (front-ptr deque)))
          deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else 
          (set-rear-ptr! deque (cadr (rear-ptr deque)))
          deque)))
