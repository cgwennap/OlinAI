;problem98.scm
;by Chaz Gwennap, Spring 2012

;solves problem 98 of 99 Lisp problems.

;Problem 98: Nonograms
;The problem can be stated as the two lists [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] and [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
;These give the "block" lengths of the rows and columns, top-to-bottom and left-to-right, respectively.
;The goal is to give the actual rectangular graph of 1s and 0s, that fit the lists provided.
;Published puzzles are larger than this example, e.g. 25 x 20, and apparently always have unique solutions.

(define print
  (lambda (var)
    (display var)
    (newline)))

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
        (cons 'x (exes (- length 1))))))
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
;(merge-ccombos (list '(1) '(2)) (list '(1) '(2))) ;returns ((1 1) (1 2) (2 1) (2 2))

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

;wraps all items in a list in lists.
(define list-to-listlist
  (lambda (listvar)
    (if (null? listvar)
        (list)
        (cons (list (car listvar)) (list-to-listlist (cdr listvar))))))
;(list-to-listlist '(1 2))

;does car for a list of rows,
;e.g (rowcar (list (list 1 2) (list 3 4))) returns (1 3)
(define rowcar
  (lambda (listlist)
    (if (null? listlist)
        listlist
        (cons (car (car listlist)) (rowcar (cdr listlist))))))
;(rowcar (list (list 1 2) (list 3 4)))

;does cdr for a list of rows
;e.g (rowcdr (list (list 1 2) (list 3 4))) returns ((2) (4))
(define rowcdr
  (lambda (listlist)
    (if (null? listlist)
        listlist
        (cons (cdr (car listlist)) (rowcdr (cdr listlist))))))
;(rowcdr (list (list 1 2) (list 3 4)))

;--- Branch evaluation functions ---
;enter in column and incomplete column, if the last elements match, then return true
(define columnmatch
  (lambda (fullcol col)
    (if (= (list-length fullcol) (list-length col))
        (equal? fullcol col)
        (columnmatch (cdr fullcol) col))))

;(columnmatch '(2 1) '(1)) ;should return true

;enter list of potential columns and incomplete column, return true if at least one match exists
(define listcolumnmatch
  (lambda (collist col)
    (if (null? collist)
        #f
        (or (columnmatch (car collist) col)
            (listcolumnmatch (cdr collist) col)))))
;(listcolumnmatch (list '(1 2) (list 2 3)) '(3)) ;should return true

;given clue and incomplete column, run listcolumnmatch
;Note: currently deprecated by validrowlist
(define cluecolmatch
  (lambda (clue col)
    (listcolumnmatch (possible-rows clue 7) col)))
;(cluecolmatch '(2 1) '(1 0 1 0 0 0)) ;should return true

;given list of rows, list of clues, and length of columns
;return true if valid combination of rows, false otherwise
(define validrowlist
  (lambda (rowlist cluelist collength)
    ;(print rowlist)
    (if (null? cluelist)
        #t
        (and (listcolumnmatch (possible-rows (car cluelist) collength) (rowcar rowlist))
             (validrowlist (rowcdr rowlist) (cdr cluelist) collength)))))
;(validrowlist (possible-rows '(2 1) 4) (list '(2 1) '(1 1) '(1 1) '(1 1)) 4) ;returns true
;(validrowlist (possible-rows '(2 1) 4) (list '(2 1) '(1 1) '(2 1) '(1 1)) 4) ;returns false because cannot have zero at (4,3)

;filters rowlists based on whether they match the column clues.
;requires list of rowlists, list of clues, and length of columns
(define filterrows
  (lambda (rowlistlist cluelist collength)
    (if (null? rowlistlist)
        (list)
        (if (validrowlist (car rowlistlist) cluelist collength)
            (cons (car rowlistlist) (filterrows (cdr rowlistlist) cluelist collength))
            (filterrows (cdr rowlistlist) cluelist collength)))))
;(filterrows (list-to-listlist (possible-rows '(1 1) 4)) (list '(2 1) '(1 1) '(1 1) '(1 1)) 4) ;should return (((1 0 1 0)) ((1 0 0 1))) 2/3 possibilities
;(filterrows (list-to-listlist (possible-rows '(1) 4)) (list '(2 1) '(2 1) '(1 1) '(1 1)) 4) ;should return ()

;---End Branch evaluation functions---

;function which generates all possible squares based on row clues.
(define generate-boards2
  (lambda (rowclues columnclues collength)
    (if (null? rowclues)
        (list)
        (if (null? (cdr rowclues))
            (list-to-listlist (possible-rows (car rowclues) (list-length columnclues)))
            (filterrows (merge-ccombos (list-to-listlist (possible-rows (car rowclues) (list-length columnclues))) (generate-boards2 (cdr rowclues) columnclues collength))
                        columnclues
                        collength)))))
;(generate-boards2 (list '(1) '(1) '(1)) (list '(1) '(1) '(1)) 3)

;function which generates all possible squares based on row clues.
(define generate-boards
  (lambda (rowclues columnclues)
    (if (null? rowclues)
        (list)
        (if (null? (cdr rowclues))
            (list-to-listlist (possible-rows (car rowclues) (list-length columnclues)))
            (filterrows (merge-ccombos (list-to-listlist (possible-rows (car rowclues) (list-length columnclues))) (generate-boards2 (cdr rowclues) columnclues (list-length rowclues)))
                        columnclues
                        (list-length rowclues))))))
;(generate-boards (list '(1) '(1) '(1)) (list '(1) '(1) '(1)))
;(generate-boards (list '(1 1) '(1 1) '(1 1) '(1 1)) (list '(1 1) '(1 1) '(1 1) '(1 1)))
(generate-boards (list '(3) '(2 1) '(3 2) '(2 2) '(6) '(1 5) '(6) '(1) '(2)) (list (list 1 2) (list 3 1) (list 1 5) (list 7 1) (list 5) (list 3) (list 4) (list 3)))
;returns (((0 x x x 0 0 0 0) (x x 0 x 0 0 0 0) (0 x x x 0 0 x x) (0 0 x x 0 0 x x) (0 0 x x x x x x) (x 0 x x x x x 0) (x x x x x x 0 0) (0 0 0 0 x 0 0 0) (0 0 0 x x 0 0 0)))

;TODO: Add function to render problem. Technically not required for problem, but would be nice.