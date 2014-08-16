(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf leaf) (cadr leaf))

(define (weight-leaf leaf) (caddr leaf))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch 
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) 
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             (cdr pair))
                  (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((not (member? symbol tree))
         (error "symbol not in the tree: " symbol))
        ((member? symbol (left-branch tree))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((member? symbol (right-branch tree))
         (cons 1 (encode-symbol symbol (right-branch tree))))))

(define (member? symbol tree)
  (if (leaf? tree) 
    (eq? symbol (symbol-leaf tree))
    (member symbol (symbols tree))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (= (length set) 1)
    (car set)
    (successive-merge 
      (adjoin-set 
        (make-code-tree (car set) (cadr set))
        (cddr set)))))

(define pairs (list
                (cons 'a 2)
                (cons 'boom 1)
                (cons 'get 2)
                (cons 'job 2)
                (cons 'na 16)
                (cons 'sha 3)
                (cons 'yip 9)
                (cons 'wah 1)))

(define message '(get a job sha na na na na na na na na 
                      get a job sha na na na na na na na na 
                      wah yip yip yip yip yip yip yip yip yip 
                      sha boom))

(length (encode message (generate-huffman-tree pairs)))

; huffman: 84
; fixed-length code: 267
