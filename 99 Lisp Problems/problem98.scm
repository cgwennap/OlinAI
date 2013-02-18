;problem98.scm
;by Chaz Gwennap, Spring 2012

;solves problem 98 of 99 Lisp problems.

;Problem 98: Find the number of elements in a list
;The problem can be stated as the two lists [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] and [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
;These give the "solid" lengths of the rows and columns, top-to-bottom and left-to-right, respectively.
;The goal is to give the actual rectangular graph of 1s and 0s, that fit the lists provided.
;Published puzzles are larger than this example, e.g. 25 x 20, and apparently always have unique solutions.

;TODO: Add function to render problem based on clues