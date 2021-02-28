#lang racket

; Used for unit testing
(require test-engine/racket-tests)

(provide create-board
         struct-out board
         struct-out board-players
         struct-out row
         struct-out spaces
         struct-out players
         struct-out players-list)

; A board is a (board list-of-player list-of-spaces number)
(struct board (players spaces turn))

; A row is (row list-of-number)
(struct row (levels))

; Spaces are (spaces list-of-row)
(struct spaces (list))

; Players are (players list-of-player
(struct players (list))

(define (create-board list-of-players list-of-rows turn)
  (board (players list-of-players) (spaces list-of-rows) turn))