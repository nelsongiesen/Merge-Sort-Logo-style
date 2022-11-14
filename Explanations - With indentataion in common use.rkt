




                 ; Merge-two is a program that takes two sorted lists and turns them into one sorted list.
                 ; We will use this technique as the building block for our sorter.
(define merge-two
  (lambda (orig-list-1 orig-list-2)
    (letrec ((helper  
              (lambda (list-1 list-2 result)
                (if
                 (empty? list-1)
                 (combine result list-2)
                 (if
                  (empty? list-2)
                  (combine result list-1)
                  (let
                      ((val-1 (first-item list-1))
                       (val-2 (first-item list-2)))
                    (if
                     (less-than? val-1 val-2)
                     (helper
                      (remove-first list-1)
                      list-2
                      (add-to-end result val-1))
                     (helper 
                      list-1 
                      (remove-first list-2)
                      (add-to-end result val-2))))))))) 
      (helper orig-list-1 orig-list-2 empty-list)))) 
                                          


              ; Feeder is a program that takes a list of sorted lists and merges them two by two into longer sorted lists.             
(define feeder
  (lambda (orig-list-of-lists)
    (letrec ((helper 
              (lambda (list-of-lists result) 
                (if
                 (less-than-two? list-of-lists) 
                 (combine result list-of-lists) 
                 (helper 
                  (remove-first (remove-first list-of-lists)) 
                  (add-to-end result (merge-two (first list-of-lists) (second list-of-lists)))))))) 
      (helper orig-list-of-lists empty-list))))





                      ; Less-than-two? is a question that asks if a list has less than two items.                       
(define less-than-two? 
  (lambda (ls)
    (if 
     (or 
      (empty? ls)
      (empty? (without-first ls)))
     yes 
     no))) 

                   ; Second-item is a selector that chooses the second item of a list.                   
(define second-item 
  (lambda (ls)
    (first-item 
     (remove-first ls)))) 

                ; Director is a program that takes a list of sorted lists and it returns a sorted list.                 
(define director
  (lambda (list-of-lists) 
    (if 
     (exactly-one? list-of-lists)
     (first list-of-lists)
     (director
      (feeder list-of-lists)))))

                    ; Exactly-one is a question that asks if a list has exactly one item in it.                    
(define exactly-one?
  (lambda (ls)
    (if
     (and
      (inverter (empty? ls))
      (empty? (without-first ls)))
     yes
     no)))

                     ; Inverter is a function that takes a yes or a no and it returns the opposite of what you give it.0                     
(define inverter not)

                    ; Listify-each is a program that takes a list. And it returns a list with each item in its own list.                    
(define listify-each
  (lambda (orig-ls)
    (do-to-each listify orig-ls)))

                  ; Do-to-each is a function that takes a function and a list. It does the function to each item of the list.                   
(define do-to-each 
  (lambda (funct ls) 
    (map funct ls))) 
   
                  ; Merge-sort is a program that takes an unsorted list and returns a sorted list.                  
(define merge-sort 
  (lambda (ls) 
    (director (listify-each ls)))) 

; Logo-style names for Scheme functions.
; Most of the functions that we need for Logo-style are already available in the Scheme dialect.

; Selectors

(define first-item car) ; Scheme dialect already has a function that chooses the first item: it is called car.
                       
(define without-first-item cdr) ; Scheme dialect already has a function that returns a list without its first item: it is called cdr.
                               
                 ; Scheme dialect doesn't have a selector that chooses the last item of a list.
                 ; So we get to do it the fun way: we make our own.
(define last-item 
  (lambda (orig-ls) 
    (letrec ((helper
              (lambda (ls) 
                (if 
                 (empty? (without-first-item ls)) 
                 (first-item ls) 
                 (helper 
                  (remove-first ls)))))) 
      (helper orig-ls)))) 

                         ; Scheme dialect doesn't have a selector that takes a list and returns the list without the last item.
                         ; So we get to do it the fun way and make our own.
(define without-last-item 
  (lambda (orig-ls) 
    (letrec ((helper 
              (lambda (ls result) 
                (if 
                 (empty? (without-first ls)) 
                 result 
                 (helper 
                  (without-first ls) 
                  (add-to-end result (first ls))))))) 
      (helper orig-ls empty-list))))
                                    

; Constructors

(define add-to-beginning cons) ; Scheme dialect already has a function that adds an item to the beginning of a list: it is called cons.

(define combine-lists append) ; Scheme dialect already has a function that takes two lists and combines them into one list: it is called append.


(define listify list) ; Scheme dialect already has a function that takes any number of items and makes them into a list: it is called list.
                     

                  ; Scheme dialect does not have a constructor that adds an item onto the end of a list.
                  ; But we can use two of the constructors that we already have to make it ourselves.
(define add-to-end
  (lambda (ls item) 
    (combine ls (listify item)))) 

; Questions

(define empty? null?) ; Scheme dialect already has a question that asks if a list is empty: it is called null?
                     
(define less-than? <) ; Scheme dialect already has a function that asks is a number is less than another number: it is called <.
                     

(define more-than? >) ; Scheme dialect already has a function that asks if a number is more than another number: it is called >.
                     
; Word-representations

(define yes #t) ; Scheme dialect already has a word that represents yes: it is called #t.
               

(define no #f) ; Scheme dialect already has a word that represents no: it is called #f.
             
(define empty-list '()) ; Scheme dialect already has a word that represents the empty list: it is '()
                       
; Common Alternate Spellings

; Alternate Selector Spellings

(define first first-item)
(define second second-item)
(define last last-item)
(define without-first without-first-item)
(define remove-first without-first-item)
(define first first-item)
(define without-last without-last-item)
(define remove-last-item without-last-item)
(define remove-last without-last-item)
(define withoutlast without-last-item)
(define withoutlastitem without-last-item)

; Alternate Constructor Spellings

(define combine combine-lists)
(define combin combine-lists)
(define combin-lists combine-lists)
(define add-to-beg add-to-beginning)
(define add-to-front add-to-beginning)
(define add-to-back add-to-end)
(define add-to-ending add-to-end)

; Alternate Question Spellings

(define greater-than? more-than?)
(define mtlist empty-list)
(define second second-item)
(define exactly-one-item? exactly-one?)


; Alternate Home-made Program Spellings

(define mergetwo merge-two)
(define two-merge merge-two)
(define twomerge merge-two)
(define merge-to merge-two)
(define merg-two merge-two)

(define feder feeder)
(define fedeer feeder)
(define feedr feeder)