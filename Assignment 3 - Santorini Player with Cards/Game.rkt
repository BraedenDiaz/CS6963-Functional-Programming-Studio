#lang racket

; Used for unit testing
(require test-engine/racket-tests)
(require json)

(require "Board.rkt")
(require "Player.rkt")

#|
           Author: Braeden Diaz
            Class: CS 6963 - Functional Programming
     Assignment 2: Santorini Player

|#

; Example Players
(define player1 (player '('(2 3) '(4 4))))
(define player2 (player '('(2 5) '(3 5))))

; Example board rows
(define row1 (row '(0 0 0 0 2)))
(define row2 (row '(1 1 2 0 0)))
(define row3 (row '(1 0 0 3 0)))
(define row4 (row '(0 0 3 0 0)))
(define row5 (row '(0 0 0 1 4)))

; Example board
(define exampleBoard (board '(player1 player2) '(row1 row2 row3 row4 row5) 18))

; Deserialize the JSON players into a list of player struct representations
(define (create-players json-players)
  (map (lambda (player-spaces)
         (let* ([card (hash-ref player-spaces 'card)]
                [tokens (hash-ref player-spaces 'tokens)])
           (player card tokens)))
       json-players))

; Deserialize the JSON spaces into a list of row struct representations
(define (create-board-rows json-spaces)
  (map (lambda (row-list)
         (row row-list))
       json-spaces))

; Check is the passed in board represents a winning move
(define (winning-move board)
  (let* ([player (list-ref (board-players board) 0)]
         [player-token1 (list-ref (player-tokens player) 0)]
         [player-token2 (list-ref (player-tokens player) 1)])
    (cond
      [(equal? (get-space-level board (list-ref player-token1 0) (list-ref player-token1 1)) 3) #t]
      [(equal? (get-space-level board (list-ref player-token2 0) (list-ref player-token2 1)) 3) #t]
      [else #f])))

#|
 A simple game playing strategy that works as follows:
     - Pick one of the players tokens randomly
     - Get a list of all the valid moves for that token
     - Pick a random valid move from the list of valid moves
     - Get a list of all the valid build spaces for the tokens new position
     - Pick a random valid build space to build on

     Note: More startegy versions may be created for the second part of the assignment.
|#
(define (moveV1 board)
  (let* ([player (list-ref (board-players board) 0)]
         [selected-token  (list-ref (player-tokens player) (random 0 2))]
         [valid-move-spaces (get-valid-move-spaces board selected-token)]
         [selected-move-space (list-ref valid-move-spaces (random 0 (length valid-move-spaces)))]
         [moved-board (move-player-token board selected-token selected-move-space)]
         [valid-build-spaces (get-valid-build-spaces moved-board selected-move-space)]
         [selected-build-space (list-ref valid-build-spaces (random 0 (length valid-build-spaces)))]
         [build-board (build-at-space moved-board selected-build-space)]
         [final-board (increment-turn build-board)])
    (cond
      [(equal? (winning-move moved-board) #t) moved-board] ; If the move was a winning move, return the moved-board
      [(= (length valid-move-spaces) 0) board] ; If there are no valid moves, return the board
      [else final-board]))) ; Otherwise, return the final-board after our move has been made and the turn is complete

#| for testing the moveV1 function (put in the body of the let*)

(displayln selected-token)
    (displayln selected-move-space)
    (displayln (player-tokens (list-ref (board-players moved-board) 0)))
    (displayln valid-build-spaces)
    (displayln selected-build-space)
    (map (lambda (row)
           (displayln (row-levels row)))
         (board-rows build-board))
|#

#|
 Game playing strategy V2.
     - Get a list of all valid moves for both of the player's tokens
     - 
|#
(define (moveV2 player)
  0)

; Simply choose a random starting position
(define (choose-starting-position)
  (let* ([row1 (random 1 6)]
         [col1 (random 1 6)]
         [row2 (random 1 6)]
         [col2 (random 1 6)])
    (list (list row1 col1) (row2 col2))))

; Serialize the game board into JSON and write it out
(define (serialize final-board)
  (let* ([players-lst (map (lambda (player)
                             (player-tokens player))
                           (board-players final-board))]
         [rows-lst (map (lambda (row)
                          (row-levels row))
                        (board-rows final-board))])
    
    (write-json (hasheq 'players players-lst 'spaces rows-lst 'turn (board-turn final-board)))))

; Read the game from the input JSON and deserialize it into
; this game's representation.
(define (read-game)
  (let* ([input (read-json)]
         [players (hash-ref input 'players)]
         [spaces (hash-ref input 'spaces)]
         [turn (hash-ref input 'turn)]
         [list-of-players (create-players players)]
         [list-of-rows (create-board-rows spaces)]
         [board (create-board list-of-players list-of-rows turn)])
    (if (= (length players) 0)
        (serialize (create-board choose-starting-position list-of-rows turn))
        (serialize (moveV1 board)))
    (flush-output)))

; Game loop
(define (game-loop)
  (read-game)
  (game-loop))

(game-loop)