#lang racket
(provide traverse)
(provide binary_search)
(provide insert_item)
(provide insert_list)
(provide treesort)
(provide treesort_compare)

; Define some binary search trees
(define i_am_root '(() 3 ()))
(define two_tree '((() 1 ()) 2 (() 3 ())))
(define bigger_tree '(((() 3 ()) 5 (() 7 ())) 10 (() 15 ())))
(define complete_tree '(((() 3 ()) 5 (() 7 ())) 10 ((() 13 ()) 15 (() 17 ()))))
(define example_tree '(((() 1 ()) 3 ((() 4 ()) 6 (() 7 ()))) 8 (() 10 ((() 13 ()) 14 ()))))
(define example_list_to_insert '(8 3 1 6 4 7 10 14 13))

; Part A: Display the contents of a binary search tree (in sorted order)
(define (traverse tree callback)
  (cond
    [(not (null? tree)) (begin
       (traverse (car tree) callback)
       (callback (cadr tree))
       (traverse (caddr tree) callback))]))
(display "Traverse & Print a tree with only root: ")
(traverse i_am_root write)
(display "\nTraverse & Print a complete tree with height two: ")
(traverse two_tree (curry printf "~a,"))
(display "\nTraverse & Print the example tree: ")
(define traverse_print (curryr traverse (curry printf "~a,"))) ;curryr is like curry, but does arguments right-to-left
(traverse_print example_tree)

; Part B: Search the tree for a value
(define (binary_search tree key)
  (cond
    [(null? tree) #f]
    [(equal? key (cadr tree)) #t]
    [(< key (cadr tree)) (binary_search (car tree) key)]
    [(> key (cadr tree)) (binary_search (caddr tree) key)]))
(display "\n\nSearch single item tree for value that exists (should be #t): ")
(binary_search i_am_root 3)
(display "Search single item tree for value that does not exists (should be #f): ")
(binary_search i_am_root 4)
(display "Search large tree for value that exists (should be #t): ")
(binary_search complete_tree 5)
(display "Search large tree for value that exists (should be #t): ")
(binary_search complete_tree 17)
(display "Search large tree for value that does not exists (should be #f): ")
(binary_search complete_tree 256)

; Part C: Insert a value into a BST
(define (insert_item tree item)
  (cond
    [(null? tree) (list '() item '())]
    [(equal? item (cadr tree)) tree]
    [(< item (cadr tree)) (list (insert_item (car tree) item) (cadr tree) (caddr tree))]
    [(> item (cadr tree)) (list (car tree) (cadr tree) (insert_item (caddr tree) item))]))
(display "\nInsert 4 into an empty/null tree. Should be (() 4 ()): ")
(insert_item '() 4)
(display "Insert 3 into a small tree where it already exists, should be (() 3 ()): ")
(insert_item i_am_root 3)
(display "Insert 15 into a bigger tree where it already exists, should be '(((() 3 ()) 5 (() 7 ())) 10 (() 15 ()))): ")
(insert_item bigger_tree 3)
(display "Insert 5 into the example tree, should be '(((() 1 ()) 3 ((() 4 (() 5 ())) 6 (() 7 ()))) 8 (() 10 ((() 13 ()) 14 ()))): ")
(define updated_tree (insert_item example_tree 5)) ;save the results
updated_tree ;then, display the results
(display "Insert 9 into the result of the previous, should be '(((() 1 ()) 3 ((() 4 (() 5 ())) 6 (() 7 ()))) 8 ((() 9 ()) 10 ((() 13 ()) 14 ()))): ")
(insert_item updated_tree 9)
(display "Insert first 2, then 1, then 3 into a null tree. Should be ((() 1 ()) 2 (() 3 ())): ")
(insert_item (insert_item (insert_item '() 2) 1) 3)

; Part D: Insert a list into a BST
(define (insert_list tree lst)
  (if (empty? lst)
     tree
     (insert_list (insert_item tree (car lst)) (cdr lst))))
; TODO make it tail recursive...WAIT it already is! cool.
(display "\nInsert list into empty tree: '(2 1 3). Should be ((() 1 ()) 2 (() 3 ())): ")
(insert_list '() '(2 1 3))
(display "Insert '(0 4) into that list. Should be (((() 0 ()) 1 ()) 2 (() 3 (() 4 ()))): ")
(insert_list two_tree '(0 4))
(display "Insert list that makes an unbalanced tree: '(1 2 3). Should be (() 1 (() 2 (() 3 ()))): ")
(insert_list '() '(1 2 3))
(display "Recreate example tree from a list. Should output #true: ")
(equal? example_tree (insert_list '() example_list_to_insert))

; Part E: Treesort
;Treesort to cout
(define (treesort_display lst)
  (traverse_print (insert_list '() lst)))
(display "\nTreesort on the example list (print). Should be 1,3,4,6,7,8,10,13,14,: ")
(treesort_display example_list_to_insert)

;Treesort to a new, ordered list
(define (treesort lst)
  (inorder_to_list (insert_list '() lst))) ;first build a tree, then use it to build a list (by visiting all nodes in order)
(define (inorder_to_list tree)
  (if (null? tree)
    '()
    (append ;Acts as a psuedo (begin)
      (inorder_to_list (car tree))
      (list (cadr tree))
      (inorder_to_list (caddr tree)))))
(display "\nTreesort on the example list (list-ify). Should be '(1 3 4 6 7 8 10 13 14): ")
(treesort example_list_to_insert)
(display "Treesort of same data should be the same, even if inserted in different order: ")
(equal? (treesort '(1 2 3)) (treesort '(2 1 3)))
(display "Treesort of same num items but different data should be, of course, false: ")
(equal? (treesort '(2 3 1 4)) (treesort '(20 30 10 40)))
(display "Treesort 'make my CPU hurt': ")
(treesort '(60 12 435 6 21 42 677 43 65 90 76 9 34 678 4 22 4 67 97 21 46 2 6 8 90 345 27 2111 64 60 0 11 -2 244 5 77 33 5 4444 60303))

; Part F: Tree sort with sorting function
(define (treesort_compare lst comparator)
  (inorder_to_list (insert_list_compare '() lst comparator)))
(define (insert_list_compare tree lst comparator)
  (if (empty? lst)
     tree
     (insert_list_compare (insert_item_compare tree (car lst) comparator) (cdr lst) comparator)))
(define (insert_item_compare tree item comparator)
  (cond
    [(null? tree) (list '() item '())]
    [(equal? item (cadr tree)) tree]
    [(comparator item (cadr tree)) (list (insert_item_compare (car tree) item comparator) (cadr tree) (caddr tree))]
    [else (list (car tree) (cadr tree) (insert_item_compare (caddr tree) item comparator))]))
(display "\nTreesort on the example list: <. Should be '(1 3 4 6 7 8 10 13 14): ")
(treesort_compare example_list_to_insert <)
(display "Treesort on the example list: >. Should be '(14 13 10 8 7 6 4 3 1): ")
(treesort_compare example_list_to_insert >)
(display "Treesort on a list of strings with (string<?): . Should be '(Apple Banana Cumquat Duran Elderberry): ")
(treesort_compare '("Elderberry" "Cumquat" "Apple" "Duran" "Banana") string<?)
(display "Reversed (string>?): . Should be '(Apple Banana Cumquat Duran Elderberry): ")
(treesort_compare '("Elderberry" "Cumquat" "Apple" "Duran" "Banana") string>?)
