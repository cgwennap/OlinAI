;problem98.scm
;by Chaz Gwennap, Spring 2012

;solves problem 98 of 99 Lisp problems.

;Problem 98: Nonograms
;The problem can be stated as the two lists [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] and [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
;These give the "solid" lengths of the rows and columns, top-to-bottom and left-to-right, respectively.
;The goal is to give the actual rectangular graph of 1s and 0s, that fit the lists provided.
;Published puzzles are larger than this example, e.g. 25 x 20, and apparently always have unique solutions.


(define list-length 
  (lambda (listvar) 
    (if (null? listvar)
        0
        (+ 1 (list-length (cdr listvar))))))

(define exes
  (lambda (length)
    (if (= length 0)
        (list)
        (cons 'x (exes (- length 1))))))
;(exes 4)

(define zeroes
  (lambda (length)
    (if (= length 0)
        (list)
        (cons 0 (zeroes (- length 1))))))
;(zeroes 1)

;function to return a list of the first list merged with each list in listlistvar
(define merge-combos
  (lambda (listvar listlistvar)
    (if (null? listlistvar)
        listvar
        (if (null? (cdr listlistvar))
            (list (append listvar (car listlistvar)))
            (append (list (append listvar (car listlistvar))) (merge-combos listvar (cdr listlistvar)))))))

;(merge-combos (list 1) (list (list 2) (list 3) (list 3 4))) ;returns ((1 2) (1 3) (1 3 4))

;Return list of potential segments of rows based on segment length and length of "solid"
;Seglength must be at least 1 greater than solidlength.
(define seg-list
  (lambda (solidlength seglength)
    (if (= seglength (+ 1 solidlength))
        (list (append (exes solidlength) '(0)))
        (cons (append (exes solidlength) (zeroes (- seglength solidlength))) (merge-combos (list 0) (seg-list solidlength (- seglength 1)))))))

;(seg-list 2 5) ;should return ((x x 0 0 0) (0 x x 0 0) (0 0 x x 0))

;sums elements in list, adds 1 per additional element beyond 1
(define cdr-length
  (lambda (listvar)
    (if (null? listvar)
        0
        (if (null? (cdr listvar))
            (car listvar)
            (+ 1 (+ (car listvar) (cdr-length (cdr listvar))))))))
(cdr-length '(2 4 4)) ;should return 12

;TODO: Generate possibilities for a given list of lengths, and a given row length.
;eg.  (possible-rows (2 1) 5) should return ((X X 0 0 X) (X X 0 X 0) (0 X X 0 X))
;It's the same problem as ((1 1) 4)
;It's the same problem as (X 0)(1 2) (0 X 0)(1 1)
;If the list has more than 1 element, the first one can be (rowlength + 3 - 2 * length (listvar)) long, then iterate through the others to find those possibilities

(define possible-rows
  (lambda (listvar rowlength)
    (if (null? listvar)
        listvar
        (if (null? (cdr listvar))
            (seg-list (car listvar) rowlength)
            (display "Recurse! (Not functional yet)")))))
;(possible-rows (list 2) 5)

;TODO: Add function to render problem based on clues
