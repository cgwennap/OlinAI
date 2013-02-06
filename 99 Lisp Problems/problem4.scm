;problem4.scm
;by Chaz Gwennap, Spring 2012

;solves problem 4 of 99 Lisp problems.

;Problem 4: Find the number of elements in a list

(define list-length (lambda (listvar) (if (null? (cdr listvar))
                                          1
                                          (list-length-counter (cdr listvar) 1))))
(define list-length-counter (lambda (listvar counter) (if (null? (cdr listvar))
                                                          (+ counter 1)
                                                          (list-length-counter (cdr listvar) (+ counter 1)))))

(list-length '(2 3 4 5))