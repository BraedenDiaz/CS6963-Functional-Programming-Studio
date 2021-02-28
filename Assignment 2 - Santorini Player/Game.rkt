#lang racket

; Used for unit testing
(require test-engine/racket-tests)
(require json)

(require "Board.rkt")
(require "Player.rkt")

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
(define exampleBoard (board (players '(player1 player2)) (spaces '(row1 row2 row3 row4 row5)) 18))

; Deserialize the JSON players into a list of player struct representations
(define (create-players json-players)
  (map (lambda (player-spaces)
         (player player-spaces))
       json-players))

; Deserialize the JSON spaces into a list of row struct representations
(define (create-board-rows json-spaces)
  (map (lambda (row-list)
         (row row-list))
       json-spaces))

#|
 A simple game playing strategy that works as follows:
     - Pick one of the players tokens randomly
     - Get a list of all the valid moves for that token
     - Pick a random valid move from the list of valid moves
     - Get a list of all the valid build spaces for the tokens new position
     - Pick a random valid build space to build on
|#
(define (moveV1 player)
  (displayln (player-tokens player)))

#|
 Game playing strategy V2.
     - Get a list of all valid moves for both of the player's tokens
     - 
|#
(define (moveV2 player)
  0)

(define (read-game)
  (let* ([input (read-json)]
         [players (hash-ref input 'players)]
         [spaces (hash-ref input 'spaces)]
         [turn (hash-ref input 'turn)]
         [list-of-players (create-players players)]
         [list-of-rows (create-board-rows spaces)]
         [board (create-board list-of-players list-of-rows turn)])
    (moveV1 (list-ref (players-list (board-players board)) 0))
    (flush-output)))

(define (game-loop)
  (read-game)
  (game-loop))

(game-loop)