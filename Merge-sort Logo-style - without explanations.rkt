; How to use the sorter:
; We use parentheses to run programs.
; But the sorter takes a list as its input.
; So we run the sorter like this: (merge-sort (listify 3 2 1 6 5 4 9 8 7))
; The inside parentheses create a list.
; And the outside parentheses run the merge-sort program on the list.
; If you type that into any Scheme-dialect repl (the same type of instant interpretter that Python uses) it will return a sorted list.
; You can have much fun testing and tinkering with it.

                  ; Merge-two is a program that takes two sorted lists and turns them into one sorted list.
                  ; We will use this technique as the building block for our sorter.
                  ; We start with two lists and find out which list's first item is lower.
                  ; We move that to the end of the result list.
                  ; If either of our lists becomes empty, we add the other list to the end of result. That's how we know we are done.
(define merge-two 
 (lambda (orig-list-1 orig-list-2) 
  (letrec ((helper 
             (lambda (list-1 list-2 result)
               (if (empty? list-1) 
                     (combine result list-2) ; And we are done. EXIT.
                     (if (empty? list-2) 
                           (combine result list-1) ; And we are done. EXIT.
                           (let ((val-1 (first-item list-1)) 
                                 (val-2 (first-item list-2))) 
                                 (if (less-than? val-1 val-2) 
                                     (helper 
                                         (remove-first list-1) 
                                         list-2 
                                         (add-to-end result val-1)) 
                                     (helper 
                                         list-1 
                                         (remove-first list-2) 
                                         (add-to-end result val-2))))))))) 
      (helper orig-list-1 orig-list-2 empty-list)))) ; This last line is called the initialization-line.
                                           ; We start our helper program with its original inputs.
                                           ; We start list-1 as orig-list-1.
                                           ; We start list-2 as orig-list-2.
                                           ; And we start result as an empty list.


               ; Feeder is a program that takes a list of sorted lists and merges them two by two into longer sorted lists.
               ; We send the first two lists to the merge-two program and add that to the end of result.
               ; We will know we are done when there are less than two lists left. We combine them with result and return that.
(define feeder 
  (lambda (orig-list-of-lists) 
    (letrec ((helper 
              (lambda (list-of-lists result) 
                       (if (less-than-two? list-of-lists) 
                           (combine result list-of-lists) ; We are done. EXIT.
                           (helper 
                             (remove-first (remove-first list-of-lists)) 
                             (add-to-end result (merge-two (first list-of-lists) (second list-of-lists)))))))) 
        (helper orig-list-of-lists empty-list)))) ; This last line is the initialization-line.
                                                  ; It starts our helper program with its original inputs.
                                                  ; We start list-of-lists as orig-list-of-lists.
                                                  ; And we start result as an empty list.


                       ; Less-than-two? is a question that asks if a list has less than two items.
                       ; We ask if the list is empty or if removing its first item makes it empty.
(define less-than-two? 
  (lambda (ls) 
    (if (or 
          (empty? ls) 
          (empty? (without-first ls))) 
        yes 
        no))) 

                    ; Second-item is a selector that chooses the second item of a list.
                    ; We remove the first item of the list. Then we select the first item of that.
(define second-item 
  (lambda (ls) 
   (first-item 
     (remove-first ls)))) 

                 ; Director is a program that takes a list of sorted lists and returns a sorted list.
                 ; We send the list-of-lists to feeder, which combines them two-by-two and sends them back.
                 ; If feeder sends us one list back, we know we are done because everything is all in one list.
                 ; If there is more than one list, we send them back to feeder.
(define director 
  (lambda (list-of-lists) 
    (if (exactly-one? list-of-lists) 
        (first list-of-lists) ; And we are done. EXIT.
        (director 
          (feeder list-of-lists))))) 

                     ; Exactly-one is a question that asks if a list has exactly one item in it.
                     ; We want to ask if the list without its first item is empty.
                     ; But we will return an error if we try to ask for an empty list without its first item.
                     ; So we have to ask if the list is empty.
                     ; If it is not empty, we will ask if removing its first item makes it empty.
                     ; We will return the answer: yes or no.
(define exactly-one? 
  (lambda (ls) 
    (if (and 
          (inverter (empty? ls)) 
          (empty? (without-first ls))) 
        yes 
        no))) 

                      ; Inverter is a function that takes a yes or a no and it returns the opposite of what you gave it.
                      ; If you give it a no, it returns a yes.
                      ; If you give it a yes, it returns a no.
(define inverter not) 

                     ; Listify-each is a program that takes a list.
                     ; It puts each item in its own list and then puts all those lists into a great big list.
(define listify-each 
  (lambda (orig-ls) 
    (do-to-each listify orig-ls))) 

                   ; Do-to-each is a function that takes a program and a list. It does the program to each item of the list.
(define do-to-each 
  (lambda (prog ls) 
    (map prog ls))) 
   
                   ; Merge-sort is a program that takes an unsorted list and returns a sorted list.
                   ; It's only job is to turn ls into a list-of-lists by listifying each item of ls.
                   ; And then giving the list-of-lists to director.
                   ; Because each list only has one item, each list is sorted.
(define merge-sort 
  (lambda (ls) 
    (director (listify-each ls)))) 

; Logo-style names for Scheme functions.
; Most of the functions that we need for Logo-style are already available in the Scheme dialect.

; Selectors

(define first-item car) ; Scheme dialect already has a function that chooses the first item: it is called car.
(define without-first-item cdr) ; Scheme dialect already has a function that returns a list without its first item: it is called cdr.

                  ; Scheme dialect doesn't have a selector that chooses the last item of a list.
                  ; So we get to do it the fun way: make our own.
                  ; If our list only has one item, we know we are done. And we return the item.
                  ; If not, we remove the first item and try again.
(define last-item 
  (lambda (orig-ls) 
    (letrec ((helper 
              (lambda (ls) 
        (if (exactly-one? ls) 
            (first-item ls) And we are done. EXIT.
            (helper 
              (remove-first ls)))))) 
      (helper orig-ls)))) ; This last line is our initialization-line: this starts helper with its original inputs.
                          ; We start ls as orig-ls.

                     ; Scheme dialect doesn't have a selector that takes a list and returns the list without the last item.
                     ; So we get to do it the fun way and make our own.
                     ; We make a helper program with two inputs: ls and result.
                     ; We start ls as the orig-ls and we start result as an empty list.
                     ; We know we are done, if ls only has one item in it. Then we return result.
                     ; If ls has more than one item, we move the first item of ls to the end of result. And we try again.
(define without-last-item 
  (lambda (orig-ls) 
    (letrec ((helper 
              (lambda (ls result) 
                    (if (exactly-one? ls) 
                        result ; And we are done. EXIT.
                        (let ((firsty (first ls))) 
                           (helper 
                              (without-first ls) 
                              (add-to-end result firsty))))))) 
      (helper orig-ls empty-list)))) ; This last line is the initialization-line.
                                     ; This starts helper with its original inputs.
                                     ; We start ls as orig-ls.
                                     ; And we start result as an empty list.

; Constructors

(define add-to-beginning cons) ; Scheme dialect already has a function that adds an item to the beginning of a list: it is called cons.
(define combine append) ; Scheme dialect already has a function that takes two lists and combines them into one list: it is called append.
(define listify list) ; Scheme dialect already has a function that takes any number of items and makes them into a list: it is called list.

                   ; Scheme dialect does not have a constructor that adds an item onto the end of a list.
                   ; But we can use two of the constructors that we already have to make it ourselves.
(define add-to-end 
  (lambda (ls item) 
    (combine ls (listify item)))) 

; Questions

(define empty? null?) ; Scheme dialect already has a question that asks if a list is empty: it is called null?.
(define less-than? <) ; Scheme dialect already has a function that asks is a number is less than another number: it is called <.
(define more-than? >) ; Scheme dialect already has a function that asks if a number is more than another number: it is called >.

; Word-representations

(define yes #t) ; Scheme dialect already has a word that represents yes: it is called #t.
(define no #f) ; Scheme dialect already has a word that represents no: it is called #f.
(define empty-list '()) ; Scheme dialect already has a word that represents the empty list: it is '()

; Common Alternate Spellings

; Alternate Selector Spellings
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
(define combine-list append)
(define add-to-beg add-to-beginning)
(define add-to-front add-to-beginning)
(define add-to-back add-to-end)

; Alternate Question Spellings

(define greater-than? more-than?)
(define exactly-one-item? exactly-one?)

; Alternate Word Representation Spellings

(define mtlist empty-list)

; Alternate Main Program Spellings

(define mergetwo merge-two)
(define two-merge merge-two)
(define twomerge merge-two)
(define merge-to merge-two)
(define merg-two merge-two)

(define feder feeder)
(define fedeer feeder)
(define feedr feeder)