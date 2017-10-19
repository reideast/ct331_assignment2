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
(provide count_instances)
(provide count_instances_tr)


(define (ins_beg el lst)
  (append (cons el '()) lst))
(ins_beg 'a '(b c d))
(ins_beg '(a b) '(b c d))

(define (ins_end el lst)
  (append lst (list el)))
(ins_end 'a '(b c d))
(ins_end '(a b) '(b c d))

(define (cout_top_level lst)
  (if (empty? lst)
      0
      (+ 1 (cout_top_level (cdr lst)))))
(printf "Should be equal 4:~a" (cout_top_level (ins_beg 'a '(b c d))))
(printf ", 5:~a~n" (cout_top_level (ins_end 'a '(b c d e))))

(define (count_instances item lst)
  (cond
    [(empty? lst) 0]
    [(cond
       [(equal? item (car lst)) (+ 1 (count_instances item (cdr lst)))]
       (else (count_instances item (cdr lst))))]))
(printf "Should be equal 1:~a" (count_instances 'a '(a b c d)))
(printf ", 2:~a" (count_instances 'a '(a b a c d)))
(printf ", 0:~a" (count_instances 'a '(b c d)))
(printf ", 5:~a" (count_instances 'a '(a a a a a)))
(printf ", 1:~a" (count_instances '(a b) '((a b) (a c) d)))
(printf ", 2:~a~n" (count_instances '(a b) '((a b) (a c) d (a b) (b a) (u u d d l r l r b a))))

(define (count_instances_tr item lst)
  (_helper_count_instances_tr item lst 0))
(define (_helper_count_instances_tr item lst total)
  (cond
    [(empty? lst) total]
    [(cond
       [(equal? item (car lst)) (_helper_count_instances_tr item (cdr lst) (+ 1 total))]
       (else (_helper_count_instances_tr item (cdr lst) total)))]))
(printf "Should be equal 1:~a" (count_instances_tr 'a '(a b c d)))
(printf ", 2:~a" (count_instances_tr 'a '(a b a c d)))
(printf ", 0:~a" (count_instances_tr 'a '(b c d)))
(printf ", 5:~a" (count_instances_tr 'a '(a a a a a)))
(printf ", 1:~a" (count_instances_tr '(a b) '((a b) (a c) d)))
(printf ", 2:~a~n" (count_instances_tr '(a b) '((a b) (a c) d (a b) (b a) (u u d d l r l r b a))))

