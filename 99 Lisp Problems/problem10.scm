;problem8.scm
;by Chaz Gwennap, Spring 2013

;solves problem 10 of 99 Lisp problems.

;Problem 10: Use the result of problem P09 to implement the so-called run-length encoding data compression method.
;Example:
;* (encode '(a a a a b c c a a d e e e e))
;((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

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

;(pack '(1 1 4 3 5 5 8 8 8 8))

;from problem4.scm
(define list-length 
  (lambda (listvar) 
    (if (null? listvar)
        0
        (+ 1 (list-length (cdr listvar))))))

;assumes uniform values in list
(define length-value 
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (list (list-length listvar) (car listvar)))))

;(length-value '(1 1 2))

;must be given a packed list (most importantly, a list of lists)
(define encode 
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (cons (length-value (car listvar)) (encode (cdr listvar))))))

(encode (pack '(1 1 4 3 5 5 8 8 8 8)))