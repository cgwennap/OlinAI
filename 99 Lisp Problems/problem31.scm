;problem31.scm
;by Chaz Gwennap, Spring 2013

;solves problem 31 of 99 Lisp problems.

;Problem 31: Determine if a given number is prime

(define is-prime
  (lambda (intvar)
    (is-prime-tracker intvar 2)))

(define is-prime-tracker
  (lambda (intvar histvar)
    (if (= 0 (modulo intvar histvar))
        #f
        (if (> (/ intvar 2) histvar)
            (is-prime-tracker intvar (+ 1 histvar))
            #t))))

(is-prime 18)