#lang racket
(provide traverse)

; Define some binary search trees
(define i_am_root '(() 3 ()))
(define two_tree '((() 1 ()) 2 (() 3 ())))
(define bigger_tree '(((() 3 ()) 5 (() 7 ())) 10 (() 15 ())))
(define complete_tree '(((() 3 ()) 5 (() 7 ())) 10 ((() 13 ()) 15 (() 17 ()))))

; Part A: Display the contents of a binary search tree (in sorted order)
(define (traverse tree callback)
  (cond
    [(not (null? tree)) (begin
       (traverse (car tree) callback)
       (callback (cadr tree))
       (traverse (caddr tree) callback))]))
(display "Traverse & Print a tree with only root\n")
(traverse i_am_root (curry fprintf (current-output-port) "~a~n"))
(display "Traverse & Print a complete tree with height 2\n")
(traverse two_tree writeln)

; Part B: Search the tree for a value
(define (binary_search tree key)
  (cond
    [(null? tree) #f]
    [else (or
        (equal? key (cadr tree))
        (binary_search (car tree) key)
        (binary_search (caddr tree) key))]))

(display "Search single item tree for value that exists (should be #t): ")
(binary_search i_am_root 3)
(display "Search single item tree for value that does not exists (should be #f): ")
(binary_search i_am_root 4)
(display "Search large tree for value that exists (should be #t): ")
(binary_search complete_tree 5)
(display "Search large tree for value that exists (should be #t): ")
(binary_search complete_tree 17)
(display "Search large tree for value that does not exists (should be #f): ")
(binary_search complete_tree 256)
