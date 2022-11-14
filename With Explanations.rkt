

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                  ; Merge-two is a program that takes two sorted lists and turns them into one sorted list.
                  ; We will use this technique as the building block for our sorter.
(define merge-two ; We use the define function to create our program, in this case it is named merge-two.
 (lambda (orig-list-1 orig-list-2) ; We use the lambda function to name our inputs: we have two named orig-list-1 and orig-list-2.
  (letrec ((helper ; We use a letrec function to create a helper program: in this case it is named helper.
   (lambda (list-1 list-2 result); We use a lambda function to name helper's inputs: we have three named list-1, list-2, and result.
                       (if ; We use an if function to set up a fork in the road. We will ask a question to choose which path we take.
                        (empty? list-1) ; We use empty? to ask if list-1 is empty.
                           (combine result list-2) ; If the answer is yes, we combine result with list-2.
                           (if ; If the answer is no, we use another if to set up another fork in the road.
                            (empty? list-2) ; We use empty? to ask if list-2 is empty.
                               (combine result list-1) ; If the answer if yes, we combine result with list-1.
                               (let ; If the answer is no, we use a let function to create local variables. In this case we have two.
                                   ((val-1 (first-item list-1)) ; val-1 is assigned to the first item of list-1.
                                     (val-2 (first-item list-2))) ; val-2 is assigned to the first item of list-2.
                                 (if ; We use another if function to set up another fork in the road.
                                     (less-than? val-1 val-2) ; We use less-than? to ask if val-1 is less than val-2.
                                     (helper ; If the answer if yes, we run helper with these inputs.
                                       (remove-first list-1) ; We remove the first-item of list-1.
                                       list-2 ; We leave list-2 unchanged.
                                       (add-to-end result val-1)) ; We add val-1 to the end of result.
                                     (helper ; If the answer is no, we run helper with these inputs.
                                       list-1 ; We leave list-1 unchanged.
                                       (remove-first list-2) ; We remove the first item from list-2.
                                       (add-to-end result val-2))))))))) ; We add val-2 to the end of result.
      (helper orig-list-1 orig-list-2 empty-list)))) ; This last line is called the initialization-line.
                                           ; We start our helper program with its original inputs: orig-list-1, orig-list-2, and an empty list.


               ; Feeder is a program that takes a list of sorted lists and merges them two by two into longer sorted lists.
(define feeder ; We use a define function to create a program, in this case it is named Feeder.
  (lambda (orig-list-of-lists) ; We use a lambda function to name our inputs. We have one named orig-list-of-lists.
    (letrec ((helper ; We use a letrec function to create helper programs. We have one named helper.
              (lambda (list-of-lists result) ; We use a lambda function to name helper's inputs: we have two named list-of-lists and result.
                       (if ; We use an if function to set up a fork in the road. We will ask a question to determine which path we take.
                        (less-than-two? list-of-lists) ; We use less-than-two? to ask if there are less than two items in list-of-lists.
                           (combine result list-of-lists) ; If the answer is yes, we combine result and list-of-lists.
                           (helper ; If the answer is no, we run helper with different inputs.
                            (remove-first (remove-first list-of-lists)) ; We remove the first two items of list-of-lists.
                            (add-to-end result (merge-two (first list-of-lists) (second list-of-lists)))))))) ; We change result.
                                                        ; We add merge-two first and second list-of-lists onto the end of result.
        (helper orig-list-of-lists empty-list)))) ; This last line is the initialization-line.
                                                  ; It starts our helper program with its original inputs.
                                                  ; In this case, we give helper orig-list-of-lists and an empty list.


                       ; Less-than-two? is a question that asks if a list has less than two items.
(define less-than-two? ; We use define to create a program. We name it less-than-two?
  (lambda (ls) ; We use lambda to name our inputs. We have one named ls.
    (if ; We use if to set up a fork in the road. We will ask a question to choose which path we take.
     (or ; We use or to combine two questions into one.
      (empty? ls) ; We use empty? to ask if ls is empty.
      (empty? (without-first ls))) ; We use empty? to ask if ls without its first element is empty.
     yes ; If the answer to either of our questions is yes, we return yes.
     no))) ; If the answer to both of our questions is no, we return no.

                    ; Second-item is a selector that chooses the second item of a list.
(define second-item ; We use define to create a program. We name it second-item.
  (lambda (ls) ; We use lambda to name our inputs: we have one named ls.
   (first-item ; We use first-item to select the first of something.
     (remove-first ls)))) ; We use remove-first to return ls without its first item.

                 ; Director is a program that takes a list of sorted lists and it returns a sorted list.
(define director ; We use define to create a program, in this case it is named Director.
  (lambda (list-of-lists) ; We use lambda to name our inputs. In this case we have one named list-of-lists.
    (if ; We use if to set up a fork in the road. We will ask a question to choose which path we will take.
     (exactly-one? list-of-lists) ; We use exactly-one? to ask if there is exactly one item in list-of-lists.
        (first list-of-lists) ; If the answer is yes, we return the first item of list-of-lists.
        (director ; If the answer is no, we run director w/ different inputs.
         (feeder list-of-lists))))) ; We change list-of-lists, by running it through the feeder.

                     ; Exactly-one is a question that asks if a list has exactly one item in it.
(define exactly-one? ; We use define to create a program. We name it exactly-one?
  (lambda (ls) ; We use lambda to name our inputs. We have one named ls.
    (if ; We use if to set up a fork in the road. We will ask a question to choose which path we take.
     (and ; We use and to combine two questions into one.
      (inverter (empty? ls)) ; We use empty? to ask if ls is empty. Then we use inverter to reverse the answer.
      (empty? (without-first ls))) ; We use without-first to return ls without its first item. Then we use empty? to ask if that is empty.
     yes ; If the answer to both our questions is yes, we return yes.
     no))) ; If the answer to either question is no, we return no.

                      ; Inverter is a function that takes a yes or a no and it returns the opposite of what you give it.
(define inverter not) ; Scheme dialect already has a function that does this: it is named not.

                     ; Listify-each is a program that takes a list. And it returns a list with each item in its own list.
(define listify-each ; We use define to create a program. In this case we name it listify-each.
  (lambda (orig-ls) ; We use lambda to name our inputs. In this case we have one named orig-ls.
    (do-to-each listify orig-ls))) ; We use the do-to-each function with inputs listify and orig-ls.

                     ; Do-to-each is a function that takes a function and a list. It does the function to each item of the list.
(define do-to-each ; We use define to name our program, we call it do-to-each.
  (lambda (funct ls) ; We use lambda to name our inputs: we have two named funct and ls.
    (map funct ls))) ; Scheme dialect already has a function that does what we need: it is called map.
   
                   ; Merge-sort is a program that takes an unsorted list and returns a sorted list.
(define merge-sort ; We use define to create a program. We name it merge-sort.
  (lambda (ls) ; We use lambda to name our inputs. In this case we have one named ls.
    (director (listify-each ls)))) ; We use listify-each to put each item in its own list. Then we run director on that.

; Logo-style names for Scheme functions.
; Most of the functions that we need for Logo-style are already available in the Scheme dialect.

; Selectors

(define first-item car) ; Scheme dialect already has a function that chooses the first item: it is called car.
                        ; To create a function with the name that we want is as simple as 1-2-3.
                        ; 1: We use define.
                        ; 2: We give the name we want to use.
                        ; 3: We give the name of the function we want to emulate.


(define without-first-item cdr) ; Scheme dialect already has a function that returns a list without its first item: it is called cdr.
                                ; To create a function with the name we want is as easy as 1-2-3.
                                ; 1: We use define.
                                ; 2: We give the name we want to use.
                                ; 3: We give the name of the function we want to emulate.

                  ; Scheme dialect doesn't have a selector that chooses the last item of a list.
                  ; So we get to do it the fun way: make our own.
(define last-item ; We use define to create a program: in this case it is named last-item.
  (lambda (orig-ls) ; We use lambda to name our inputs: in this case we have one named orig-ls.
    (letrec ((helper ; We use letrec to create helper programs: in this case we have one named helper.
              (lambda (ls) ; We use lambda to name helper's inputs: in this case we have one named ls.
        (if ; We use if to set up a fork in the road.
         (empty? (without-first-item ls)) ; We use empty? to ask a question: without its first item, if ls empty?
          (first-item ls) ; If the answer is yes, we return the first item of ls.
          (helper ; If the answer is no, we run helper w/ different inputs.
           (remove-first ls)))))) ; We change ls, by removing its first item.
      (helper orig-ls)))) ; This last line is our initialization-line: this starts helper with its original inputs: in this case: orig-ls.

                     ; Scheme dialect doesn't have a selector that takes a list and returns the list without the last item.
                     ; So we get to do it the fun way and make our own.
(define without-last-item ; We use define to create a program: in this case we name it without-last-item.
  (lambda (orig-ls) ; We use lambda to name our inputs: in this case we have one: named orig-ls
    (letrec ((helper ; We use letrec to create helper programs: in this case we have one named helper.
              (lambda (ls result) ; We use lambda to name helper's inputs: in this case we have two: ls and result.
                       (if ; We use if to set up a fork in the road.
                        (empty? (without-first ls)) ; We use empty? to ask the question: if (without-first ls) empty?
                                   result ; If the answer is yes, we return result.
                                   (helper ; If the answer is no, we run helper with different inputs.
                                    (without-first ls) ; We change ls, by making it without-first ls.
                                    (add-to-end result (first ls))))))) ; We change result, by making it result with the first ls added to end.
      (helper orig-ls empty-list)))) ; This last line is the initialization-line.
                                     ; This starts helper with its original inputs: orig-ls and mtlist

; Constructors

(define add-to-beginning cons) ; Scheme dialect already has a function that adds an item to the beginning of a list: it is called cons.
                               ; To create a function with the name we want is as easy as 1-2-3.
                               ; 1: We use define.
                               ; 2: We give the name we want to use.
                               ; 3: We give the name of the function we want to emulate.

(define combine-lists append) ; Scheme dialect already has a function that takes two lists and combines them into one list: it is called append.
                      ; To create a function with the name we want is as easy as 1-2-3.
                      ; 1: We use define.
                      ; 2: We give the name we want to use.
                      ; 3: We give the name of the function we want to emultate.

(define listify list) ; Scheme dialect already has a function that takes any number of items and makes them into a list: it is called list.
                      ; To create a function with the name we want is as easy as 1-2-3.
                      ; 1: We use define.
                      ; 2: We give the name we want to use.
                      ; 3: We give the name of the program we want to emulate.

                   ; Scheme dialect does not have a constructor that adds an item onto the end of a list.
                   ; But we can use two of the constructors that we already have to make it ourselves.
(define add-to-end ; We use define to create a program: in this case we name it add-to-end.
  (lambda (ls item) ; We use lambda to name our inputs: in this case we have two named ls and item.
    (combine ls (listify item)))) ; We combine ls with the listified version of item. Walang.

; Questions

(define empty? null?) ; Scheme dialect already has a question that asks if a list is empty: it is called null?
                      ; To create a function with the name we want to use is as easy as 1-2-3.
                      ; We use define.
                      ; We give the name we want to use.
                      ; We give the name of the function we want to immitate.

(define less-than? <) ; Scheme dialect already has a function that asks is a number is less than another number: it is called <.
                      ; Creating a function with the name we want to use is an easy as 1-2-3.
                      ; 1: We use define.
                      ; 2: We give the name we want to use.
                      ; 3: We give the name of the function we want to immitate.

(define more-than? >) ; Scheme dialect already has a function that asks if a number is more than another number: it is called >.
                      ; Creating a function with the name we want is as easy as 1-2-3.
                      ; 1: We use define.
                      ; 2: We give the name we want to use.
                      ; 3: We give the name of the function we want to immitate.

; Word-representations

(define yes #t) ; Scheme dialect already has a word that represents yes: it is called #t.
                ; To create a function with the name we want to use is as easy as 1-2-3.
                ; 1: We use define.
                ; 2: We give the name we want to use.
                ; 3; We give the name of the function we want to immitate.

(define no #f) ; Scheme dialect already has a word that represents no: it is called #f.
               ; To create a function with the name we want to use is as easy as 1-2-3.
               ; 1: We use define.
               ; 2: We give the name we want to use.
               ; 3: We give the name of the function we want to immitate.

(define empty-list '()) ; Scheme dialect already has a word that represents the empty list: it is '()
                        ; Creating a function with the same name we want to use is as easy as 1-2-3.
                        ; 1: We use define
                        ; 2: We give the name we want to use.
                        ; 3: We give the name of the program we want to immitate.

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
(define combine-lists append)
(define add-to-beg add-to-beginning)
(define add-to-front add-to-beginning)
(define add-to-back add-to-end)

; Alternate Question Spellings


(define combine-lists append)

(define greater-than? more-than?)

(define mtlist empty-list)

(define second second-item)

(define exactly-one-item? exactly-one?)

(define last last-item)
(define without-first without-first-item)
(define remove-first without-first)
(define first first-item)
(define without-last without-last-item)

(define without-last-item without-last)
(define withoutlast without-last)
(define withoutlastitem without-last)

(define mergetwo merge-two)
(define two-merge merge-two)
(define twomerge merge-two)
(define merge-to merge-two)
(define merg-two merge-two)

(define feder feeder)
(define fedeer feeder)
(define feedr feeder)