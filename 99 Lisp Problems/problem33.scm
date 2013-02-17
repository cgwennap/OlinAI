;problem33.scm
;by Chaz Gwennap, Spring 2013

;solves problem 33 of 99 Lisp problems.

;Problem 33: Determine whether two positive integer numbers are coprime.
;Two numbers are coprime if their greatest common divisor equals 1.

;onevar MUST be greater than twovar!
(define coprime
  (lambda (onevar twovar)
    (if (= twovar 0)
        (= onevar 1)
        (coprime twovar (modulo onevar twovar)))))

(coprime 1071 462) ;should return #f
(coprime 1071 5) ;should return #t