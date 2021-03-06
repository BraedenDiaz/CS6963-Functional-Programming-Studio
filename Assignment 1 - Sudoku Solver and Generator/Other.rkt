#lang racket

#|
  NOTE: This file does not run and is here only for reference.
  
  This file contains a bunch of random codes of my first attempts
  at the Sudoku program and to study Racket.
|#

; First attempt
#|
(define (solve grid)
  (map (lambda (rowLst)
         (let ([rowIdx (index-of grid rowLst)])
         (map (lambda (value)
                (let ([colIdx (index-of rowLst value)])
                  (display colIdx)))
              rowLst)))
       grid))
|#

#|
(define (printGrid grid)
  (map (lambda (rowLst)
         (map-with-index (lambda (num idx)
                           num)
                         rowLst 0))
       grid))
|#

(define (solve2 grid)
  (map-with-index (lambda (rowLst rowIdx)
         (map-with-index (lambda (value colIdx)
                  (cond
                    [(equal? value 0)
                     (map-until-valid (range 1 10) grid rowIdx colIdx)]
                    [else value]))
              rowLst 0))
       grid 0))

;(solve2 exampleGrid)

(define (solve3 grid rowIdx colIdx)
  (cond
    [(equal? rowIdx 9) grid]
    [else (solve3 (map-row (lst-at-row grid rowIdx) grid rowIdx colIdx 1) (+ rowIdx 1) 0)]))

#|
(define (map-row rowLst grid rowIdx colIdx rangeStart)
  (cond
    [(empty? rowLst) grid]
    [else (if (equal? (first rowLst) 0)
              (let ([value (map-until-valid (range rangeStart 10) grid rowIdx colIdx)])
                (cond
                  [(equal? value -1) (map-row rowLst grid rowIdx (- colIdx 1) (element-at grid rowIdx (- colIdx 1)))]
                  [else (map-row (rest rowLst) (grid-set grid rowIdx colIdx (map-until-valid (range 1 10) grid rowIdx colIdx)) rowIdx (+ colIdx 1))]))
              (map-row (rest rowLst) grid rowIdx (+ colIdx 1)))]))
|#

(define (map-row grid rowIdx colIdx backtrackLst rangeStart)
  (cond
    [(equal? colIdx 9) grid]
    [else (if (equal? (element-at grid rowIdx colIdx) 0)
              (let ([value (map-until-valid (range rangeStart 10) grid rowIdx colIdx)])
                (cond
                  [(equal? value -1) (map-row (grid-set grid rowIdx (first (reverse backtrackLst)) 0) rowIdx (first (reverse backtrackLst)) (reverse (rest (reverse backtrackLst))) (+ (element-at grid rowIdx (first (reverse backtrackLst))) 1))]
                  [else (map-row (grid-set grid rowIdx colIdx value) rowIdx (+ colIdx 1) (append backtrackLst (list colIdx)) 1)]))
              (map-row grid rowIdx (+ colIdx 1) backtrackLst 1))]))