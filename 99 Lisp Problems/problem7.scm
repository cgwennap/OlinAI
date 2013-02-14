;problem7.scm
;by Chaz Gwennap, Spring 2013

;solves problem 7 of 99 Lisp problems.

;Problem 7: Flatten a nested list structure
;Example:
;* (my-flatten '(a (b (c d) e)))
;(A B C D E)

(define reverse-list-bad
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (cons (reverse-list-bad (cdr listvar)) (cons (car listvar) '())))))

(define testvar (reverse-list-bad '(1 2 4 8 16))) ;returns (((((() 16) 8) 4) 2) 1)

(define flatten
  (lambda (var)
    (if (null? var)
        var
        (if (list? var)
            (append (flatten (car var)) (flatten (cdr var)))
            (list var)))))

(flatten testvar) ;in combination with reverse-list-bad, reverses a list

