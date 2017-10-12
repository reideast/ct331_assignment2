#lang racket

;This is an example implementation of ins_beg,
;It obviously doesn't do what it should, so you
;can edit this function to get started.
;
;Please note the provide function is necessary
;for the unit tests to work. Please include a
;(provide) for each function you write in your
;submitted assignment.
;
;You may delete these comments!

(provide ins_beg)
(provide ins_end)
(provide cout_top_level)

(define (ins_beg el lst)
  (append (cons el '()) lst))

(define (ins_end el lst)
  (append lst (list el)))

(define (cout_top_level lst)
  (if (empty? lst)
      0
      (+ 1 (cout_top_level (cdr lst)))))

(ins_beg 'a '(b c d))
(ins_beg '(a b) '(b c d))

(ins_end 'a '(b c d))
(ins_end '(a b) '(b c d))

(cout_top_level (ins_beg 'a '(b c d)))
(cout_top_level (ins_end 'a '(b c d e)))