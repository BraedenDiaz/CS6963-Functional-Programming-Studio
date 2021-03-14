#lang racket

(provide struct-out player
         struct-out player-card
         struct-out player-tokens)

; A player is (player (string/symbol list-of-tokens))
; A token is a list-of-number coordinates (row col)
(struct player (card tokens))