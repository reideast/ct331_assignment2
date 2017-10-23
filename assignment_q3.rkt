#lang racket

(define i_am_root '(() 3 ()))
(define two_tree '((() 1 ()) 2 (() 3 ())))
(define bigger_tree '(((() 3 ()) 5 (() 7 ())) 10 (() 15 ())))
(define complete_tree '(((() 3 ()) 5 (() 7 ())) 10 ((() 13 ()) 15 (() 17 ()))))

(define (traverse tree callback)
  (begin
    (printf "at tree=~a~n" tree)
  (cond
    [(equal? tree '())]
    [else (begin
       (traverse (car tree) callback)
       (callback (cadr tree))
       (traverse (caddr tree) callback)
       )])
    (printf "done with tree=~a~n" tree)
    ))

(display "Traverse & Print a tree with only root\n")
(traverse i_am_root (curry fprintf (current-output-port) "~a~n"))

(display "Traverse & Print a complete tree with height 2\n")
(traverse two_tree writeln)