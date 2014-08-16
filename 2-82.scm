(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((op (get op type-tags)))
      (if op
        (apply op (map contents args))
        ((define (convert to-type)
           (let ((converted-types 
                   (map (lambda (from-type)
                          (if (eq? from-type to-type)
                            to-type
                            (let ((converter (get-coercion 
                                               from-type 
                                               to-type)))
                              (if converter
                                (converter from-type)
                                #f)))))))
             (if (apply and converted-types)
               converted-types
               #f)))
         (define (try-coercion types)
           (if (null? types)
             (error "No method for these types"
                    (list op type-tags))
             (let ((converted-types (convert (car types))))
               (if (and converted-types 
                        (get op (converted-types))) 
                 (apply op converted-types)
                 (iter (cdr types))))))
         (try-coercion type-tags))))))
