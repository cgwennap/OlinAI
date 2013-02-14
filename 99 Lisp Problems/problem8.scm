;problem8.scm
;by Chaz Gwennap, Spring 2013

;solves problem 8 of 99 Lisp problems.

;Problem 8: Eliminate consecutive duplicates of list elements.
;Example:
;* (compress '(a a a a b c c a a d e e e e))
;(A B C A D E)

(define compress
  (lambda (listvar)
    (if (null? (cdr listvar))
        listvar
        (if (= (car listvar) (car (cdr listvar)))
            (compress (cdr listvar))
            (cons (car listvar) (compress (cdr listvar)))))))

(compress '(1 1 1 4 2 2 3 3))