#lang racket

(provide all-defined-out
         struct-out player
         struct-out player-tokens)

; A player is (player list-of-list)
; A token is a list-of-number coordinates (row col)
(struct player (tokens))