;problem61.scm
;by Chaz Gwennap, Spring 2013

;solves problem 61 and 61A of 99 Lisp problems.

;Problem 61: Count the leaves of a binary tree
;Problem 61A: Collect the leaves of a binary tree into a flat list
;Both solutions presume no lists are values in leaves

(define count-leaves
  (lambda (var)
    (if (null? var)
        0
        (if (list? var)
            (+ (count-leaves (car var)) (count-leaves (cdr var)))
            1))))

(define leaves
  (lambda (var)
    (if (null? var)
        var
        (if (list? var)
            (append (leaves (car var)) (leaves (cdr var)))
            (list var)))))

(count-leaves (list 1 (list 2 (list) (list 3 (list) '())) (list)))  
(leaves (list 1 (list 2 (list) (list 3 (list) '())) (list)))


