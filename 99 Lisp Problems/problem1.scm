;problem1.scm
;by Chaz Gwennap, Spring 2012

;solves problem 1 of 99 Lisp problems.

;problem 1: Find the last box of a list.
;Example:
;* (my-last '(a b c d))
;(D)

(define find-last (lambda (var) (if (null? (cdr var))
                                    (car var)
                                    (find-last (cdr var)))))
(find-last (list 2 3 4))

