;; a. infinite loop
;;
;; b. No.
;;

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((op (get op type-tags)))
      (if op
        (apply op (map contents args))
        (if (and (= (length args) 2) 
                 (not (eq? (car type-tags) (cadr type-tags))))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((type1->type2 (get-coercion type1 type2))
                  (type2->type1 (get-coercion type2 type1)))
              (cond ((type1->type2 
                       (apply-generic op (type1->type2 a1) a2))
                     (type2->type1 
                       (apply-generic op a1 (type2->type1 a2)))
                     (else "No method for these types"
                           (list op type-tags))))))
          (else "No method for these types"
                (list op type-tags)))))))
