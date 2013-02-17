;problem35.scm
;by Chaz Gwennap, Spring 2013

;solves problem 35 of 99 Lisp problems.

;Problem 35: Determine the prime factors of a given positive integer.
;Construct a flat list containing the prime factors in ascending order.
;Example:
;* (prime-factors 315)
;(3 3 5 7)

(define prime-factors
  (lambda (intvar)
    (if (= intvar 1)
        (list)
        (append (list (is-prime-mod intvar)) (prime-factors (/ intvar (is-prime-mod intvar)))))))

(define is-prime-mod
  (lambda (intvar)
    (define divisor (is-prime-tracker intvar 2))
    (if (= divisor 1)
        intvar
        divisor)))

(define is-prime-tracker
  (lambda (intvar histvar)
    (if (= 0 (modulo intvar histvar))
        histvar
        (if (> (/ intvar 2) histvar)
            (is-prime-tracker intvar (+ 1 histvar))
            1))))

(prime-factors 315)