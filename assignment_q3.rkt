#lang racket
(provide traverse)
(provide binary_search)
(provide insert_item)
;(provide insert_list)
;(provide treesort)
;(provide treesort_compare)

; Define some binary search trees
(define i_am_root '(() 3 ()))
(define two_tree '((() 1 ()) 2 (() 3 ())))
(define bigger_tree '(((() 3 ()) 5 (() 7 ())) 10 (() 15 ())))
(define complete_tree '(((() 3 ()) 5 (() 7 ())) 10 ((() 13 ()) 15 (() 17 ()))))
(define example_tree '(((() 1 ()) 3 ((() 4 ()) 6 (() 7 ()))) 8 (() 10 ((() 13 ()) 14 ()))))

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
(display "\nSearch single item tree for value that exists (should be #t): ")
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
;  (begin (printf "Inserting into tree=~a key=~a~n" tree item)
  (cond
    [(null? tree) (list '() item '())]
    [(equal? item (cadr tree)) tree]
    [(< item (cadr tree)) (list (insert_item (car tree) item) (cadr tree) (caddr tree))]
    [(> item (cadr tree)) (list (car tree) (cadr tree) (insert_item (caddr tree) item))]))
(display "Insert 4 into an empty/null tree. Should be (() 4 ()): ")
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
(display "Insert first 2, then 1, then 3 into a null tree. Should be ((() 1 ()) 2 (() 3 ())): ");
(insert_item (insert_item (insert_item '() 2) 1) 3)

; Part D: Insert a list into a BST
