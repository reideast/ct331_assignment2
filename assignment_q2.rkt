#lang racket

(provide ins_beg)
(provide ins_end)
(provide cout_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)

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
    [(equal? item (car lst)) (+ 1 (count_instances item (cdr lst)))]
    [else (count_instances item (cdr lst))]))
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
    [(equal? item (car lst)) (_helper_count_instances_tr item (cdr lst) (+ 1 total))]
    [else (_helper_count_instances_tr item (cdr lst) total)]))
(printf "Should be equal 1:~a" (count_instances_tr 'a '(a b c d)))
(printf ", 2:~a" (count_instances_tr 'a '(a b a c d)))
(printf ", 0:~a" (count_instances_tr 'a '(b c d)))
(printf ", 5:~a" (count_instances_tr 'a '(a a a a a)))
(printf ", 1:~a" (count_instances_tr '(a b) '((a b) (a c) d)))
(printf ", 2:~a~n" (count_instances_tr '(a b) '((a b) (a c) d (a b) (b a) (u u d d l r l r b a))))

(define (count_instances_deep item lst)
  (cond
    [(empty? lst) 0]
    [(equal? item (car lst)) (+ 1 (count_instances_deep item (cdr lst)))]
    [(list? (car lst)) (+ (count_instances_deep item (car lst)) (count_instances_deep item (cdr lst)))]
    [else (count_instances_deep item (cdr lst))]))
(printf "Should be equal 1:~a" (count_instances_deep 'a '(a b c d)))
(printf ", 2:~a" (count_instances_deep 'a '(a b a c d)))
(printf ", 0:~a" (count_instances_deep 'a '(b c d)))
(printf ", 5:~a" (count_instances_deep 'a '(a a a a a)))
(printf ", 2:~a" (count_instances_deep 'a '((a b) (a c) d)))
(printf ", 5:~a" (count_instances_deep 'a '((a b) (a c) d (a b) (b a) (u u d d l r l r b a))))
(printf ", 6:~a~n" (count_instances_deep 'a '((a b) (a c) d (a b) (b a) a b c ((u u) ((d d) (l r l r (b a)))))))