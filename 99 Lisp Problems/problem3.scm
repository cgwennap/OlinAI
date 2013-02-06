;problem3.scm
;by Chaz Gwennap, Spring 2012

;solves problem 3 of 99 Lisp problems.

;Problem 3: Find the K'th element of a list.
;The first element in the list is number 1.
;Example:
;* (element-at '(a b c d e) 3)
;C

(define element-at (lambda (listvar) (if (= index 1)
                                               (car listvar)
                                               (element-at (cdr listvar) (- index 1)))))
(element-at '(a b c d e) 3)