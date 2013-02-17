;problem9.scm
;by Chaz Gwennap, Spring 2013

;solves problem 9 of 99 Lisp problems.

;Problem 9: Pack consecutive duplicates of list elements into separate lists.
;Example:
;* (pack '(a a a a b c c a a d e e e e))
;((A A A A) (B) (C C) (A A) (D) (E E E E))

;returns rest of list that is different from first item in list
(define find-different
  (lambda (listvar)
    (cond
      ((null? listvar) listvar)
      ((null? (cdr listvar)) (list))
      ((= (car listvar) (car (cdr listvar))) (find-different (cdr listvar)))
      (else (cdr listvar)))))

;(find-different '(1 4))

;creates list with first element and any subsequent elements that are the same as the first
(define start-list
  (lambda (listvar)
    (cond
      ((null? listvar) (list))
      ((null? (cdr listvar)) listvar)
      ((= (car listvar) (car (cdr listvar))) (cons (car listvar) (start-list (cdr listvar))))
      (else (list (car listvar))))))

;(start-list '(1 1 1 1 4 2 2 3 3))

(define pack
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (cons (start-list listvar) (pack (find-different listvar))))))

(pack '(1 1 4 3 5 5 8 8 8 8))

;;Below is interesting, but not useful;;

;find first non-repeating item in list
(define find-nonrepeating
  (lambda (listvar)
    (cond
      ((null? listvar) listvar)
      ((null? (cdr listvar)) listvar)
      ((= (car listvar) (car (cdr listvar))) (cond
                                               ((null? (cdr (cdr listvar))) (list))
                                               ((= (car listvar) (car (cdr (cdr listvar)))) (find-nonrepeating (cdr listvar)))
                                               (else (car (cdr (cdr listvar))))))
      (else (car listvar)))))

;(find-nonrepeating '(1 1 1 1 4 2 2 3 3))