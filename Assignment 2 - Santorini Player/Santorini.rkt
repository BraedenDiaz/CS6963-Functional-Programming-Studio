#lang racket

; Used for unit testing
(require test-engine/racket-tests)
(require json)

(struct board (players spaces turn))

(struct players (list-of-players))

(struct spaces (list-of-rows))

;(struct turn (turn-number))

(struct row (levels))

;(struct level (level-number))

(struct player (tokens))

;(struct space (row column))

(define player1 (player '('(2 3) '(4 4))))
(define player2 (player '('(2 5) '(3 5))))

(define row1 (row '(0 0 0 0 2)))
(define row2 (row '(1 1 2 0 0)))
(define row3 (row '(1 0 0 3 0)))
(define row4 (row '(0 0 3 0 0)))
(define row5 (row '(0 0 0 1 4)))


(define exampleBoard (board (players '(player1 player2)) (spaces '(row1 row2 row3 row4 row5)) 18))

(define (create-players json-players)
  (map (lambda (player-spaces)
         (player player-spaces))
       json-players))

(define (create-board-rows json-spaces)
  (map (lambda (row-list)
         (row row-list))
       json-spaces))

(define (create-board list-of-players list-of-rows turn)
  (board (players list-of-players) (spaces list-of-rows) turn))

(define (read-board)
  (let* ([input (read-json)]
         [players (hash-ref input 'players)]
         [spaces (hash-ref input 'spaces)]
         [turn (hash-ref input 'turn)]
         [list-of-players (create-players players)]
         [list-of-rows (create-board-rows spaces)]
         [board (create-board list-of-players list-of-rows turn)])
    (displayln board)
    (flush-output)))

(define (game-loop)
  (read-board)
  (game-loop))

(game-loop)