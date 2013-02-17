;problem55.scm
;by Chaz Gwennap, Spring 2013

;solves problem 55 of 99 Lisp problems.

;Problem 55: Construct completely balanced binary trees
;Write a predicate istree which returns true if and only if its argument is a list representing a binary tree.
;Example:
;* (cbal-tree (a (nil) nil))

(define cbal-tree
  (lambda (intvar)
    (if (= 0 intvar)
        (list)
        (list 'x (cbal-tree (- intvar 1)) (cbal-tree (- intvar 1))))))

(cbal-tree 2)

