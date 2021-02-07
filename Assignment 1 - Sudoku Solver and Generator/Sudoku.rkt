#lang racket

; Used for unit testing
(require test-engine/racket-tests)


#|
         Author: Braeden Diaz
          Class: CS 6963 - Functional Programming
     Assignment: Sudoku

     Tip: Use a small window when running the program so the
     board prints out nicely as output is not prettified by
     this program.

     Solver:
          - Backtracking algorithm
          - Only works on 3x3 boards

     Generator:
          - Only generates 3x3 boards
          - May generate boards with multiple solutions
|#

(define exampleGrid '((5 3 0 0 7 0 0 0 0)
                      (6 0 0 1 9 5 0 0 0)
                      (0 9 8 0 0 0 0 6 0)
                      (8 0 0 0 6 0 0 0 3)
                      (4 0 0 8 0 3 0 0 1)
                      (7 0 0 0 2 0 0 0 6)
                      (0 6 0 0 0 0 2 8 0)
                      (0 0 0 4 1 9 0 0 5)
                      (0 0 0 0 8 0 0 7 9)))

(define exampleGridSolution '((5 3 4 6 7 8 9 1 2)
                              (6 7 2 1 9 5 3 4 8)
                              (1 9 8 3 4 2 5 6 7)
                              (8 5 9 7 6 1 4 2 3)
                              (4 2 6 8 5 3 7 9 1)
                              (7 1 3 9 2 4 8 5 6)
                              (9 6 1 5 3 7 2 8 4)
                              (2 8 7 4 1 9 6 3 5)
                              (3 4 5 2 8 6 1 7 9)))

(define exampleGrid2 '((8 3 0 1 0 0 6 0 5)
                       (0 0 0 0 0 0 0 8 0)
                       (0 0 0 7 0 0 9 0 0)
                       (0 5 0 0 1 7 0 0 0)
                       (0 0 3 0 0 0 2 0 0)
                       (0 0 0 3 4 0 0 1 0)
                       (0 0 4 0 0 8 0 0 0)
                       (0 9 0 0 0 0 0 0 0)
                       (3 0 2 0 0 6 0 4 7)))

(define exampleGrid2Solution '((8 3 7 1 9 4 6 2 5)
                               (5 4 9 6 2 3 7 8 1)
                               (6 2 1 7 8 5 9 3 4)
                               (2 5 6 8 1 7 4 9 3)
                               (4 1 3 5 6 9 2 7 8)
                               (9 7 8 3 4 2 5 1 6)
                               (1 6 4 2 7 8 3 5 9)
                               (7 9 5 4 3 1 8 6 2)
                               (3 8 2 9 5 6 1 4 7)))

; Get the element at the specified grid row and column
(define (element-at grid x y)
  (list-ref (list-ref grid x) y))

(check-expect (element-at exampleGrid 2 2) 8)

; Get a specific row in the grid
(define (getRow grid row)
  (list-ref grid row))

(check-expect (getRow exampleGrid 0) '(5 3 0 0 7 0 0 0 0))
(check-expect (getRow exampleGrid 4) '(4 0 0 8 0 3 0 0 1))

; Get a specific column in the grid
(define (getColumn grid column)
  (map (lambda (lst)
         (list-ref lst column))
       grid))

(check-expect (getColumn exampleGrid 0) '(5 6 0 8 4 7 0 0 0))
(check-expect (getColumn exampleGrid 1) '(3 0 9 0 0 0 6 0 0))

;; ----------------------------------------

; Check if the provided value is allowed in the provided row based on Sudoku rules
(define (allowedInRow grid row value)
  (if(member value (getRow grid row))
     #f
     #t))

(check-expect (allowedInRow exampleGrid 0 7) #f)
(check-expect (allowedInRow exampleGrid 4 1) #f)
(check-expect (allowedInRow exampleGrid 4 5) #t)

;; ----------------------------------------

; Check if the provided value is allowed in the provided column based on Sudoku rules
(define (allowedInColumn grid column value)
  (if (member value (getColumn grid column))
      #f
      #t))

(check-expect (allowedInColumn exampleGrid 0 8) #f)
(check-expect (allowedInColumn exampleGrid 4 1) #f)
(check-expect (allowedInColumn exampleGrid 4 5) #t)

;; ----------------------------------------

; Get the subgrid for the provided row index and row column
(define (getSubgrid grid rowIdx colIdx)
  (map (lambda (lst) (take (list-tail lst (* (floor (/ colIdx 3)) 3)) 3))
       (take (list-tail grid (* (floor (/ rowIdx 3)) 3)) 3)))

(check-expect (getSubgrid exampleGrid 8 5) '((0 0 0) (4 1 9) (0 8 0)))
(check-expect (getSubgrid exampleGrid 5 5) '((0 6 0) (8 0 3) (0 2 0)))
(check-expect (getSubgrid exampleGrid 0 0) '((5 3 0) (6 0 0) (0 9 8)))
(check-expect (getSubgrid exampleGrid 7 2) '((0 6 0) (0 0 0) (0 0 0)))

(define (allowedInSubgrid subgrid value)
  (if (member value (flatten subgrid))
      #f
      #t))

(check-expect (allowedInSubgrid (getSubgrid exampleGrid 8 5) 9) #f)
(check-expect (allowedInSubgrid (getSubgrid exampleGrid 8 5) 3) #t)
(check-expect (allowedInSubgrid (getSubgrid exampleGrid 0 0) 1) #t)
(check-expect (allowedInSubgrid (getSubgrid exampleGrid 7 2) 6) #f)
(check-expect (allowedInSubgrid (getSubgrid exampleGrid 7 2) 5) #t)
(check-expect (allowedInSubgrid (getSubgrid exampleGrid 5 5) 2) #f)

;; ----------------------------------------

; A map function also providing the index of each element during iteration
; I found out that this can be done with 'for' afterwards, so I kept this.
(define (map-with-index func lst idx)
  (cond
    [(empty? lst) empty]
    [else (cons (func (first lst) idx)
                (map-with-index func (rest lst) (+ 1 idx)))]))

; Set the element at the provided row index and column index to the provided value.
; Functional Note: This function is still functional as it returns a new grid with
; new value while leaving the old grid intact.
(define (grid-set grid rowIdx colIdx value)
  (map-with-index (lambda (rowLst idx)
                    (cond
                      [(equal? idx rowIdx) (list-set rowLst colIdx value)]
                      [else rowLst]))
                  grid 0))

(define testGrid    '((5 3 1 0 7 0 0 0 0)
                      (6 0 0 1 9 5 0 0 0)
                      (0 9 8 0 0 0 0 6 0)
                      (8 0 0 0 6 0 0 0 3)
                      (4 0 0 8 0 3 0 0 1)
                      (7 0 0 0 2 0 0 0 6)
                      (0 6 0 0 0 0 2 8 0)
                      (0 0 0 4 1 9 0 0 5)
                      (0 0 0 0 8 0 0 7 9)))

(define testGrid2    '((5 3 0 0 7 0 0 0 0)
                      (6 0 0 1 9 5 0 0 0)
                      (0 9 8 0 0 0 0 6 0)
                      (8 0 0 0 6 0 0 0 3)
                      (4 0 0 8 5 3 0 0 1)
                      (7 0 0 0 2 0 0 0 6)
                      (0 6 0 0 0 0 2 8 0)
                      (0 0 0 4 1 9 0 0 5)
                      (0 0 0 0 8 0 0 7 9)))

(check-expect (grid-set exampleGrid 0 2 1) testGrid)
(check-expect (grid-set exampleGrid 4 4 5) testGrid2)

; Check if the provided value is allowed at the provided row index and column
; index based on all Sudoku rules.
(define (isAllowed? grid rowIdx colIdx value)
  (and (allowedInRow grid rowIdx value)
       (allowedInColumn grid colIdx value)
       (allowedInSubgrid (getSubgrid grid rowIdx colIdx) value)))

; Places a valid number in the range (1 - 9) in a specific cell based on Sudoku rules
(define (map-until-valid lst grid rowIdx colIdx)
  (cond
    [(empty? lst) -1]
    [else (cond
            [(isAllowed? grid rowIdx colIdx (first lst))
             (first lst)]
            [else (map-until-valid (rest lst) grid rowIdx colIdx)])]))

; Main solve function that solves a Sudoku board by using backtracking.
; Backtracking utilizes a backtrackLst that holds pairs of row and column
; indecies containing the last cell we filled in case we need to backtrack.
(define (solve grid rowIdx colIdx backtrackLst rangeStart)
  (cond
    [(equal? rowIdx 9) grid]
    [else
     (cond
       [(equal? colIdx 9) (solve grid (+ rowIdx 1) 0 backtrackLst rangeStart)]
       [else (if (equal? (element-at grid rowIdx colIdx) 0)
                 (let ([value (map-until-valid (range rangeStart 10) grid rowIdx colIdx)])
                   (cond
                     [(equal? value -1) (solve (grid-set grid (car (first (reverse backtrackLst))) (cdr (first (reverse backtrackLst))) 0)
                                                (car (first (reverse backtrackLst)))
                                                (cdr (first (reverse backtrackLst)))
                                                (reverse (rest (reverse backtrackLst)))
                                                (+ (element-at grid (car (first (reverse backtrackLst))) (cdr (first (reverse backtrackLst)))) 1))]
                     [else (solve (grid-set grid rowIdx colIdx value) rowIdx (+ colIdx 1) (append backtrackLst (list (cons rowIdx colIdx))) 1)]))
                 (solve grid rowIdx (+ colIdx 1) backtrackLst rangeStart))])]))



(check-expect (solve exampleGrid 0 0 '() 1) exampleGridSolution)
(check-expect (solve exampleGrid2 0 0 '() 1) exampleGrid2Solution)

(test)

(displayln "\nSolved Example Grid:")
(solve exampleGrid 0 0 '() 1)

(displayln "")

(displayln "Solved Example Grid 2:")
(solve exampleGrid2 0 0 '() 1)

(displayln "")

;; ----------------------------------------

; Generate a single list (row) of numbers satisfying the Sudoku row rules.
; This is used to create the first row of an empty Sudoku board which we
; will solve to generate the whole board.
(define (generateRandomValidNumberLst n lst)
  (let ([randomNum (random 1 10)])
    (cond
      [(equal? n 0) lst]
      [(not (member randomNum lst)) (generateRandomValidNumberLst (- n 1) (append lst (list randomNum)))]
      [else (generateRandomValidNumberLst n lst)])))

; Generates a grid of zeros with the first row
; containg a valid Sudoku row.
(define (generateZerosGrid)
  (map (lambda (v)
         (cond
           [(equal? v 1) (generateRandomValidNumberLst 9 '())]
           [else (build-list 9 (lambda (x) (* x 0)))]))
       (range 1 10)))

; Generate a filled Sudoku grid by solving the passed in zeros grid
(define (fillGrid grid)
  (solve grid 0 0 '() 1))

; Remove n random numbers from a Sudoku grid.
; The bigger the n, the more difficult the problem.
; Note: This function does not check if there is still
; only one solution to the Sudoku board. Thus, multiple
; solutions may be possible.
(define (removeRandomNumbersFromGrid grid n)
  (let ([randomRow (random 1 9)])
    (let ([randomCol (random 1 9)])
      (cond
        [(equal? n 0) grid]
        [(not (equal? (element-at grid randomRow randomCol) 0)) (removeRandomNumbersFromGrid (grid-set grid randomRow randomCol 0)
                                                                                             (- n 1))]
        [else (removeRandomNumbersFromGrid grid n)]))))

; Generate a Sudoku board using the helper functions
(define (generateSudokuBoard)
  (removeRandomNumbersFromGrid (fillGrid (generateZerosGrid)) (random 20 65)))

(displayln "Generate Sudoku Board:")
(generateSudokuBoard)
(displayln "\nGenerate Sudoku Board:")
(generateSudokuBoard)
(displayln "\nGenerate Sudoku Board:")
(generateSudokuBoard)