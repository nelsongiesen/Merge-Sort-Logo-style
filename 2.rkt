(define tester-1 (list 3 2 1 6 5 4 9 8 7))
(define tester-2 (list 3 2 1 3 2 1 5 4 3 5 4 3))

(define remove-first cdr)
(define remove-first-item cdr)
(define first car)
(define first-item car)
(define empty? null?)
(define add-to-beginning cons)
(define add-to-beg cons)
(define add-to-front cons)
(define empty-list '())
(define less-than? <)
(define more-than? >)
(define greater-than >)
(define combine-lists append)
(define combine append)
(define listify list)


(define less-than-two?
  (lambda (ls)
  (if (or (empty? ls)
          (empty? (remove-first ls)))
      #t
      #f)))
(define second
  (lambda (ls)
    (first (remove-first ls))))
(define second-item second)
(define exactly-one?
  (lambda (ls)
    (if (and (not (empty? ls))
             (empty? (remove-first ls)))
        #t
        #f)))
(define exactly-one-item? exactly-one?)

(define last-item
  (lambda (ls)
    (if (empty? (remove-first ls))
        (first ls)
        (last-item (remove-first ls)))))

(define last last-item)


(define add-to-end
  (lambda (ls item)
    (if (empty? ls)
        (list item)
        (add-to-beginning (first-item ls)
                          (add-to-end (remove-first ls) item)))))


(define remove-last
  (lambda (ls)
    (letrec ((helper (lambda (ls result)
                       (if (empty? (remove-first ls))
                                   result
                                   (helper (remove-first ls)
                                           (add-to-end result (first ls)))))))
      (helper ls '()))))

(define remove-last-item remove-last)
(define removelast remove-last)
(define removelastitem remove-last-item)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(define merge-two
  (lambda (list-a list-b)
    (letrec ((helper (lambda (list-1 list-2 result)
                       (if (empty? list-1)
                           (combine result list-2)
                           (if (empty? list-2)
                               (combine result list-1)
                               (let ((val-1 (first-item list-1))
                                     (val-2 (first-item list-2)))
                                 (if (less-than? val-1 val-2)
                                     (helper (remove-first list-1) list-2 (add-to-end result val-1))
                                     (helper list-1 (remove-first list-2) (add-to-end result val-2)))))))))
      (helper list-a list-b empty-list))))

(define mergetwo merge-two)
(define two-merge merge-two)
(define twomerge merge-two)
(define merge-to merge-two)
(define merg-two merge-two)

(define feeder
  (lambda (list-of-lists)
    (letrec ((helper (lambda (l-of-l result)
                       (if (less-than-two? l-of-l)
                           (combine result l-of-l
                           (helper (remove-first (remove-first l-of-l))
                                   (add-to-end result (merge-two (first l-of-l) (second l-of-l))))))))
        (helper list-of-lists empty-list))))

(define feder feeder)
(define fedeer feeder)
(define feedr feeder)

(define director
  (lambda (list-of-lists)
    (if (exactly-one? list-of-lists)
        (first list-of-lists)
        (director (feeder list-of-lists)))))



(define each-item-gets-own-list
  (lambda (ls)
    (letrec ((helper (lambda (ls result)
                       (if (empty? ls)
                           result
                           (helper (remove-first ls) (add-to-end result (listify (first ls))))))))
      (helper ls empty-list))))


(define merge-sort
  (lambda (ls)
    (director (each-item-gets-own-list ls))))




                  
                           























    








