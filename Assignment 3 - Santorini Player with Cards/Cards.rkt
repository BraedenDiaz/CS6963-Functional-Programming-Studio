#lang racket

(require "Player.rkt")
(require "Board.rkt")

(provide apollo
         artemis
         atlas
         dementer
         hephastus
         minotaur
         pan
         prometheus)

; Returns a list of boards of valid move positions including opponent positions
(define (apollo board token-pos)
  (let* ([token-level (get-token-level board token-pos)]
         [current-player-tokens (player-tokens (list-ref (board-players board) 0))]
         [opponent-player-tokens (player-tokens (list-ref (board-players board) 1))])
    (map (lambda (intermediate)
           (remove (first intermediate) intermediate)) ; remove the level value from the final list
         (filter (lambda (level-space)
                   (let * ([level (car level-space)]
                           [space (cdr level-space)])
                     (cond
                       ; Make sure the current move space is within the bounds of the token and
                       ; doesn't already contain other tokens for the current player and is not
                       ; a level 4 space.
                       [(and (and (and (or (= level (+ token-level 1))
                                           (<= level token-level))
                                       (equal? (member space current-player-tokens) #f))
                                  (< level 4))
                             (> level -1)) #t]
                       [else #f])))
                 (get-8-spaces-levels board token-pos)))))

; Returns a list of boards of the valid move spaces
; The provided board should be the board after the player's first move.
(define (artemis moved-board token-pos)
  (get-valid-move-spaces moved-board token-pos))

; Returns a board with the build-pos set to level 4
(define (atlas moved-board build-pos)
  (let* ([build-pos-row (list-ref build-pos 0)]
         [build-pos-col (list-ref build-pos 1)])
    (struct-copy board moved-board
                 [rows (for/list ([i (in-naturals 1)]
                                  [rowi (board-rows moved-board)])
                         (if (equal? i build-pos-row)
                             (struct-copy row rowi
                                          [levels (for/list ([i (in-naturals 1)]
                                                             [leveli (row-levels rowi)])
                                                    (if (equal? i build-pos-col)
                                                        4
                                                        leveli))])
                             rowi))])))

; Returns a list of valid build spaces minus the first build space
(define (dementer build-board token-pos first-build-pos)
  (remove first-build-pos (get-valid-build-spaces build-board token-pos)))

; Returns a board with the first-build-pos built on one more time as long
; as it's not already a level 4 space.
(define (hephastus build-board token-pos first-build-pos)
  (cond
    [(< (get-token-level build-board first-build-pos) 4) (build-at-space build-board first-build-pos)]
    [else build-board]))

(define (minotaur board token-pos)
  (display "TODO"))

; Returns a list of valid winning spaces to move down two or more levels to.
(define (pan board token-pos)
  (let* ([token-level (get-token-level board token-pos)])
    (filter (lambda (level-space)
              (let* ([level (car level-space)]
                     [space (cdr level-space)])
                (if (>= (abs (- token-level level)) 2)
                    #t
                    #f)))
            (get-8-spaces-levels board token-pos))))

; Returns a list of valid move spaces after the player first builds
(define (prometheus build-board token-pos)
  (let* ([token-level (get-token-level build-board token-pos)]
         [current-player-tokens (player-tokens (list-ref (board-players build-board) 0))]
         [opponent-player-tokens (player-tokens (list-ref (board-players build-board) 1))])
    (map (lambda (intermediate)
           (remove (first intermediate) intermediate)) ; remove the level value from the final list
         (filter (lambda (level-space)
                   (let * ([level (car level-space)]
                           [space (cdr level-space)])
                     (cond
                       ; Make sure the current move space is within the bounds of the token and
                       ; doesn't already contain other tokens for the current player and is not
                       ; a level 4 space.
                       [(and (and (and (and (equal? (member space current-player-tokens) #f))
                                       (< level 4))
                                  (> level -1))
                             (<= level token-level)) #t]
                       [else #f])))
                 (get-8-spaces-levels build-board token-pos)))))