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
