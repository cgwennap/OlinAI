;problem54.scm
;by Chaz Gwennap, Spring 2013

;solves problem 54 of 99 Lisp problems.

;Problem 54: Check whether a given term represents a binary tree
;Write a predicate istree which returns true if and only if its argument is a list representing a binary tree.
;Example:
;* (istree (a (b nil nil) nil))
;T
;* (istree (a (b nil nil)))
;NIL

(define list-length 
  (lambda (listvar) 
    (if (null? listvar)
        0
        (+ 1 (list-length (cdr listvar))))))

(define is-tree
  (lambda (listvar)
    (if (null? listvar)
        #t
        (and (= 3 (list-length listvar))
             (is-tree (car (cdr listvar)))
             (is-tree (car (cdr (cdr listvar))))))))

(is-tree (list 1 (list 1 (list) (list 1 (list) '())) (list)))
