;problem5.scm
;by Chaz Gwennap, Spring 2013

;solves problem 5 of 99 Lisp problems.

;Problem 5: Reverse a list

(define reverse-list
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (append (reverse-list (cdr listvar)) (cons (car listvar) '())))))

(reverse-list '(1 2 4 8 16))

;;Deprecated code;;

;(define switch-two 
;  (lambda (listvar)
;    (if (null? (cdr listvar))
;        listvar
;        (cons (car (cdr listvar)) (cons (car listvar) '())))))
;
;(switch-two '(1 2))