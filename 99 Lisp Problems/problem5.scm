;problem5.scm
;by Chaz Gwennap, Spring 2013

;solves problem 5 of 99 Lisp problems.

;Problem 5: Reverse a list

;so has to recurse and return first element of list at back
;what if we had something that has two list inputs?

;XXX: Doesn't work, use Ian's implementation?

(define switch-items (lambda (first second) (list second first)))
(define list-reverse (lambda (listvar) (if (null? (cdr listvar))
                                           (car listvar)
                                           ()
                       ))
(switch-items 3 1)