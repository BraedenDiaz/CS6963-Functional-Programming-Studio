#lang racket

; Used for unit testing
(require test-engine/racket-tests)
(require json)

(require "Board.rkt")
(require "Player.rkt")
(require "Cards.rkt")

#|
           Author: Braeden Diaz
            Class: CS 6963 - Functional Programming
     Assignment 2: Santorini Player

|#

; Example Players
(define player1 (player "Artemis" '('(2 3) '(4 4))))
(define player2 (player "Prometheus" '('(2 5) '(3 5))))

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
           (player (string->symbol card) tokens)))
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

; Apply card action to the random strategy
(define (perform-action-of card original-board selected-token valid-move-spaces selected-move-space moved-board valid-build-spaces selected-build-space build-board)
  (case card
    [(Apollo) (let* ([valid-move-spaces (apollo original-board selected-token)]
                     [selected-move-space (list-ref valid-move-spaces (random 0 (length valid-move-spaces)))]
                     [moved-board (move-player-token original-board selected-token selected-move-space)]
                     [valid-build-spaces (get-valid-build-spaces moved-board selected-move-space)]
                     [selected-build-space (list-ref valid-build-spaces (random 0 (length valid-build-spaces)))]
                     [build-board (build-at-space moved-board selected-build-space)])
                build-board)]
    [(Artemis) (let* ([valid-move-spaces2 (artemis moved-board selected-token)]
                      [selected-move-space2 (list-ref valid-move-spaces2 (random 0 (length valid-move-spaces2)))]
                      [moved-board2 (move-player-token moved-board selected-token selected-move-space2)]
                      [valid-build-spaces (get-valid-build-spaces moved-board2 selected-move-space2)]
                      [selected-build-space (list-ref valid-build-spaces (random 0 (length valid-build-spaces)))]
                      [build-board (build-at-space moved-board2 selected-build-space)])
                 build-board)]
    [(Atlas) (let* ([build-board (atlas moved-board selected-build-space)])
               build-board)]
    [(Demeter) (let* ([selected-build-space2 (list-ref (dementer build-board selected-token selected-build-space)
                                                       (random 0 (length valid-build-spaces)))]
                      [build-board2 (build-at-space moved-board selected-build-space2)])
                 build-board2)]
    [(Hephastus) (let* ([build-board2 (hephastus build-board selected-token selected-build-space)])
                   build-board2)]
    [(Minotaur) 0]
    [(Pan) (let* ([valid-pan-spaces (pan original-board selected-token)])
             (if (> (length valid-pan-spaces) 1)
                 #t
                 (let* ([selected-move-space (list-ref valid-move-spaces (random 0 (length valid-move-spaces)))]
                        [moved-board (move-player-token original-board selected-token selected-move-space)]
                        [valid-build-spaces (get-valid-build-spaces moved-board selected-move-space)]
                        [selected-build-space (list-ref valid-build-spaces (random 0 (length valid-build-spaces)))]
                        [build-board (build-at-space moved-board selected-build-space)])
                   build-board)))]
    [(Prometheus) (let* ([valid-build-spaces (get-valid-build-spaces original-board selected-token)]
                         [selected-build-space (list-ref valid-build-spaces (random 0 (length valid-build-spaces)))]
                         [build-board (build-at-space original-board selected-build-space)]
                         [valid-move-spaces (prometheus build-board selected-token)]
                         [selected-move-space (list-ref valid-move-spaces (random 0 (length valid-move-spaces)))]
                         [moved-board (move-player-token build-board selected-token selected-move-space)]
                         [valid-build-spaces2 (get-valid-build-spaces moved-board selected-move-space)]
                         [selected-build-space2 (list-ref valid-build-spaces2 (random 0 (length valid-build-spaces2)))]
                         [build-board2 (build-at-space moved-board selected-build-space2)])
                    moved-board)]))

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
         [card (player-card player)]
         [selected-token  (list-ref (player-tokens player) (random 0 2))]
         [valid-move-spaces (get-valid-move-spaces board selected-token)]
         [selected-move-space (list-ref valid-move-spaces (random 0 (length valid-move-spaces)))]
         [moved-board (move-player-token board selected-token selected-move-space)]
         [valid-build-spaces (get-valid-build-spaces moved-board selected-move-space)]
         [selected-build-space (list-ref valid-build-spaces (random 0 (length valid-build-spaces)))]
         [build-board (build-at-space moved-board selected-build-space)]
         [card-board (perform-action-of card board selected-token valid-move-spaces selected-move-space moved-board valid-build-spaces selected-build-space build-board)]
         [final-board build-board])
    (cond
      [(equal? (winning-move moved-board) #t) (increment-turn moved-board)] ; If the move was a winning move, return the moved-board
      [(= (length valid-move-spaces) 0) (exit 0)] ; If there are no valid moves, simply exit
      [else (if (= (random 0 2) 0)
                (increment-turn final-board)
                (increment-turn card-board))]))) ; Otherwise, return the final-board or card-board after our move has been made and the turn is complete

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
    (list (list row1 col1) (list row2 col2))))

; Serialize the game board into JSON and write it out
(define (serialize final-board)
  (let* ([players-lst (map (lambda (player)
                             (hash 'card (symbol->string (player-card player)) 'tokens (player-tokens player)))
                           (board-players final-board))]
         [rows-lst (map (lambda (row)
                          (row-levels row))
                        (board-rows final-board))])
    
    (write-json (hasheq 'players (reverse players-lst) 'spaces rows-lst 'turn (board-turn final-board)))))

(define (setup array)
  (write-json (list (second array) (hash-set (first array) 'tokens (choose-starting-position)))))

; Read the game from the input JSON and deserialize it into
; this game's representation.
(define (read-game)
  (let* ([input (read-json)])
    (if (hash? input)
        (let* ([players (hash-ref input 'players)]
               [spaces (hash-ref input 'spaces)]
               [turn (hash-ref input 'turn)]
               [list-of-players (create-players players)]
               [list-of-rows (create-board-rows spaces)]
               [board (create-board list-of-players list-of-rows turn)])
          (serialize (moveV1 board)) ; (serialize (moveV1 board))
          (flush-output))
        (setup input))))

; Game loop
(define (game-loop)
  (read-game)
  (game-loop))

(game-loop)