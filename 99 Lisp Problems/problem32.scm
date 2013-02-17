;problem32.scm
;by Chaz Gwennap, Spring 2013

;solves problem 32 of 99 Lisp problems.

;Problem 32: Determine whether two positive integer numbers are coprime.
;Two numbers are coprime if their greatest common divisor equals 1.

;onevar MUST be greater than twovar!
(define gcd1
  (lambda (onevar twovar)
    (if (= twovar 0)
        onevar
        (gcd1 twovar (modulo onevar twovar)))))

(gcd1 1071 462) ;should return 21