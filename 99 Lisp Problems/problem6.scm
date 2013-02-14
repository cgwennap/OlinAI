;problem6.scm
;by Chaz Gwennap, Spring 2013

;solves problem 6 of 99 Lisp problems.

;Problem 6: Determine if list is a palindrome 
;A palindrome can be read forward or backward; e.g. (x a m a x).

(define reverse-list
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (append (reverse-list (cdr listvar)) (cons (car listvar) '())))))

;(equal? '(1 2 4 2 1) (reverse-list '(1 2 4 2 1))) proof of concept

(define is-palindrome
  (lambda (listvar)
    (equal? listvar (reverse-list listvar))))

(is-palindrome '(1 4 6 4 1))