#lang racket

(provide all-defined-out
         struct-out player
         struct-out player-tokens)

; A player is (player list-of-list)
(struct player (tokens))