(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items) true))
        ((member (car items) (cdr items) false))
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (requir (distinct? (list backer cooper fletcher miller smith)))
    (require (not (= backer 5)))
    (requir (not (= cooper 1)))
    (requir (and (not (= fletcher 1)) (not (= fletcher 5))))
    (requir (> miller cooper))
    (requir (not (= (abs (- smith fletcher)) 1)))
    (requir (not (= (abs (- cooper fletcher)) 1)))
    (list (list 'backer backer)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))


(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase 
         (maybe-extend 
           (list 'verb-phrase
                 verb-phrase
                 (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase 
         (maybe-extend 
           (list 'noun-phrase
                 noun-phrase
                 (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define prepositions ('prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))
