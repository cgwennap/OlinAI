;problem98.scm
;by Chaz Gwennap, Spring 2012

;solves problem 98 of 99 Lisp problems.

;Problem 98: Nonograms
;The problem can be stated as the two lists [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] and [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
;These give the "block" lengths of the rows and columns, top-to-bottom and left-to-right, respectively.
;The goal is to give the actual rectangular graph of 1s and 0s, that fit the lists provided.
;Published puzzles are larger than this example, e.g. 25 x 20, and apparently always have unique solutions.


(define list-length 
  (lambda (listvar) 
    (if (null? listvar)
        0
        (+ 1 (list-length (cdr listvar))))))

;can vary 'x without effecting results of code, used because of contrasts with 0
(define exes
  (lambda (length)
    (if (= length 0)
        (list)
        (cons 1 (exes (- length 1))))))
;(exes 4)

(define zeroes
  (lambda (length)
    (if (= length 0)
        (list)
        (cons 0 (zeroes (- length 1))))))
;(zeroes 1)

;function to return a list of the first list merged with each list in listlistvar
;used to collapse a tree into a list of paths. eg seg-list, seg-gen,
(define merge-combos
  (lambda (listvar listlistvar)
    (if (null? listlistvar)
        listvar
        (if (null? (cdr listlistvar))
            (list (append listvar (car listlistvar)))
            (append (list (append listvar (car listlistvar))) (merge-combos listvar (cdr listlistvar)))))))
;(merge-combos (list 1) (list (list 2) (list 3) (list 3 4))) ;returns ((1 2) (1 3) (1 3 4))

;function to return a list of each item in listlist1 merged with each item in listlist2
;currently used in generate-boards
(define merge-ccombos
  (lambda (listlist1 listlist2)
    (if (null? listlist1)
        listlist1
        (append (merge-combos (car listlist1) listlist2) (merge-ccombos (cdr listlist1) listlist2)))))
;(merge-ccombos (list '(1) '(2)) (list '(1) '(2)))

;Return list of potential segments of rows based on segment length and length of block.
;Used at end of row generator to vary space before and after block
(define seg-list
  (lambda (solidlength seglength)
    (if (= seglength solidlength)
        (list (exes solidlength))
        (cons (append (exes solidlength) (zeroes (- seglength solidlength))) (merge-combos (list 0) (seg-list solidlength (- seglength 1)))))))

;(seg-list 2 5) ;should return ((x x 0 0 0) (0 x x 0 0) (0 0 x x 0) (0 0 0 x x))

;only add zeroes before exes, then one after the x.
;Used in middle of row generator, as to avoid overlap (only vary space to left, except when last block in row)
(define seg
  (lambda (solidlength seglength)
    (append (zeroes (- seglength (+ 1 solidlength))) (append (exes solidlength) '(0)))))
;(seg 2 5) ;should return (0 0 x x 0)

;sums elements in list, adds 1 per additional element beyond 1
(define cdr-length
  (lambda (listvar)
    (if (null? listvar)
        0
        (if (null? (cdr listvar))
            (car listvar)
            (+ 1 (+ (car listvar) (cdr-length (cdr listvar))))))))
;(cdr-length '(2 4 4)) ;should return 12

;gives all combos of sums given largest number and sum of the two numbers
(define pet-results
  (lambda (shortlength sum)
    (if (= shortlength 0)
        (list)
        (cons (list shortlength (- sum shortlength)) (pet-results (- shortlength 1) sum)))))
;(pet-results 5 16) ;should return ((5 11) (4 12) (3 13) (2 14) (1 15))

;generates a segment of the row, recursively moves around if it has the space
(define seg-gen
  (lambda (listvar rowlength shortlength)
    (if (= shortlength (car listvar))
        (list)
        (append (merge-combos (seg (car listvar) shortlength) (possible-rows (cdr listvar) (- rowlength shortlength))) (seg-gen listvar rowlength (- shortlength 1))))))
;(seg-gen (list 2) 16 5)

;Generates possibilities for a given list of lengths, and a given row length.
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
            (seg-gen listvar rowlength (- rowlength (cdr-length (cdr listvar))))))))
;(possible-rows (list 2 1) 9) ;excerpt from example nonogram

;;;Auxiliary functions from problem 10;;;
;returns rest of list that is different from first item in list
(define find-different
  (lambda (listvar)
    (cond
      ((null? listvar) listvar)
      ((null? (cdr listvar)) (list))
      ((eq? (car listvar) (cadr listvar)) (find-different (cdr listvar)))
      (else (cdr listvar)))))

;(find-different '(1 4))

;creates list with first element and any subsequent elements that are the same as the first
(define start-list
  (lambda (listvar)
    (cond
      ((null? listvar) (list))
      ((null? (cdr listvar)) listvar)
      ((eq? (car listvar) (cadr listvar)) (cons (car listvar) (start-list (cdr listvar))))
      (else (list (car listvar))))))

;(start-list '(1 1 1 1 4 2 2 3 3))

(define pack
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (cons (start-list listvar) (pack (find-different listvar))))))

;(pack '(1 1 4 3 5 5 8 8 8 8))

;assumes uniform values in list
(define length-value 
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (list (list-length listvar) (car listvar)))))
;(length-value '(1 1 2)) ;will return (3 1)

;must be given a packed list (most importantly, a list of lists)
(define encode 
  (lambda (listvar)
    (if (null? listvar)
        listvar
        (cons (length-value (car listvar)) (encode (cdr listvar))))))
;;;End Auxilliary problem10.scm functions;;; 

(define encode-clue
  (lambda (listlistvar)
    (if (null? listlistvar)
        (list)
        (if (= 1 (cadr (car listlistvar)))
            (cons (car (car listlistvar)) (encode-clue (cdr listlistvar)))
            (encode-clue (cdr listlistvar))))))
;(encode-clue (encode (pack '(1 1 0 1 0 0 0 0 0))));(pack '('x 'x 0 'x 0 0 0 0 0)))

;function which converts row to nonogram clue.
(define row-to-clue
  (lambda (row)
    (encode-clue (encode (pack row)))))

;(row-to-clue '(1 1 0 1 0 0 0 0 0))

;(combo-boards '('(1) '(2)) '('(3) '(4))) should result in (('(1) '(3))('(1) '(4))('(2) '(3))('(2) '(4))
;(define combo-boards
;  (lambda (list 1 var)))

;wraps all items in a list in lists.
(define list-to-listlist
  (lambda (listvar)
    (if (null? listvar)
        (list)
        (cons (list (car listvar)) (list-to-listlist (cdr listvar))))))
(list-to-listlist '(1 2))

;function which generates all possible squares based on row clues.
;FIXME: Significant memory/performance issues for squares beyond 6x6. Resolve by filtering out incorrect solutions retroactively.
(define generate-boards
  (lambda (rowclues columnclues)
    (if (null? rowclues)
        (list)
        (if (null? (cdr rowclues))
            (list-to-listlist (possible-rows (car rowclues) (list-length columnclues)))
            (merge-ccombos (list-to-listlist (possible-rows (car rowclues) (list-length columnclues))) (generate-boards (cdr rowclues) columnclues))))))
;(list-length (generate-boards (list '(3) '(2 1) '(3 2) '(2 2) '(6) '(1 5) '(6) '(1) '(2)) (list 1 2 3 4 5 6 7 8)))

;does car for a list of rows,
;e.g (rowcar (list (list 1 2) (list 3 4))) returns (1 3)
(define rowcar
  (lambda (listlist)
    (if (null? listlist)
        listlist
        (cons (car (car listlist)) (rowcar (cdr listlist))))))
(rowcar (list (list 1 2) (list 3 4)))

;does cdr for a list of rows
;e.g (rowcdr (list (list 1 2) (list 3 4))) returns ((2) (4))
(define rowcdr
  (lambda (listlist)
    (if (null? listlist)
        listlist
        (cons (cdr (car listlist)) (rowcdr (cdr listlist))))))
(rowcdr (list (list 1 2) (list 3 4)))

;TODO: Add function to evaluate legality of columns, to enable branch pruning and save memory.
;enter in column and incomplete column, if the last elements match, then return true
(define columnmatch
  (lambda (fullcol col)
    (if (= (list-length fullcol) (list-length col))
        (equal? fullcol col)
        (columnmatch (cdr fullcol) col))))

(columnmatch '(2 1) '(1)) ;should return true

;enter list of potential columns and incomplete column, return true if at least one match exists
(define listcolumnmatch
  (lambda (collist col)
    (if (null? collist)
        #f
        (or (columnmatch (car collist) col)
            (listcolumnmatch (cdr collist) col)))))
(listcolumnmatch (list '(1 2) (list 2 3)) '(3)) ;should return true

;given clue and incomplete column, run listcolumnmatch
;FIXME: make rowlength not be set to 7
(define cluecolmatch
  (lambda (clue col)
    (listcolumnmatch (possible-rows clue 7) col)))
(cluecolmatch '(2 1) '(1 0 1 0 0 0)) ;should return true

;TODO: Add function to render problem