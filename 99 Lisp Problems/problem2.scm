;problem2.scm
;by Chaz Gwennap, Spring 2012

;solves problem 2 of 99 Lisp problems.

;problem 2: Find the last two boxes of a list.
;Example:
;* (my-last-two '(a b c d))
;(C D)

(define find-last-two (lambda (var) (if (null? (cdr (cdr var)))
                                    var
                                    (find-last-two (cdr var)))))
(find-last-two (list 2 3 4 5))
