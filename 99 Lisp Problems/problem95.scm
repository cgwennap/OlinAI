;problem95.scm
;by Chaz Gwennap, Spring 2012

;solves problem 95 of 99 Lisp problems.

;Problem 95: English number words
;Example: 175 must be written as one-seven-five.

(define str-num
  (lambda (intvar)
    (if (< intvar 10)
        (str-digit intvar)
        (string-append (str-num (floor (/ intvar 10))) (string-append "-" (str-digit (modulo intvar 10)))))))

(define str-digit
  (lambda (digitvar)
    (case digitvar
      ((0) "zero")
      ((1) "one")
      ((2) "two")
      ((3) "three")
      ((4) "four")
      ((5) "five")
      ((6) "six")
      ((7) "seven")
      ((8) "eight")
      ((9) "nine")
      (else "NaD"))))

(str-num 175)

