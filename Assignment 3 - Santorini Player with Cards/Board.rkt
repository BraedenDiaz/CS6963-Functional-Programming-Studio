#lang racket

; Used for unit testing
(require test-engine/racket-tests)

(require "Player.rkt")

(provide create-board
         struct-out board
         struct-out board-players
         struct-out board-rows
         struct-out board-turn
         struct-out row
         struct-out row-levels
         get-space-level
         get-valid-move-spaces
         get-valid-build-spaces
         move-player-token
         build-at-space
         increment-turn
         get-token-level
         get-8-spaces-levels)

(define INVALID-LEVEL -1)
(define MAX-LEVEL 4)
(define MIN-ROW 1)
(define MAX-ROW 5)
(define MIN-COL 1)
(define MAX-COL 5)

; A board is a (board list-of-players list-of-rows number)
(struct board (players rows turn))

; A row is (row list-of-number)
(struct row (levels))

(define (create-board list-of-players list-of-rows turn)
  (board list-of-players list-of-rows turn))

(define (get-board-row board row)
  (list-ref (board-rows board)
            (- row 1)))

; Get the level for the space at row col on board
(define (get-space-level board row col)
  (cond
    [(and (and (>= row MIN-ROW) (<= row MAX-ROW))
          (and (>= col MIN-COL) (<= col MAX-COL)))
     (list-ref (row-levels
                (get-board-row board row))
               (- col 1))]
    [else INVALID-LEVEL]))

; Get the level of the space for a specific token position
(define (get-token-level board token-pos)
  (get-space-level board (list-ref token-pos 0) (list-ref token-pos 1)))

(define (move-player-token current-board selected-token-pos new-space)
  (struct-copy board current-board
               [players (for/list ([i (in-naturals 1)]
                                   [playeri (board-players current-board)])
                          (if (equal? i 1)
                              (struct-copy player playeri
                                           [tokens (for/list ([i (in-naturals 1)]
                                                                     [tokeni (player-tokens playeri)])
                                                            (if (equal? tokeni selected-token-pos)
                                                                new-space
                                                                tokeni))])
                              playeri))]))

(define (build-at-space current-board build-pos)
  (let* ([build-pos-row (list-ref build-pos 0)]
         [build-pos-col (list-ref build-pos 1)])
    (struct-copy board current-board
                 [rows (for/list ([i (in-naturals 1)]
                                  [rowi (board-rows current-board)])
                         (if (equal? i build-pos-row)
                             (struct-copy row rowi
                                          [levels (for/list ([i (in-naturals 1)]
                                                             [leveli (row-levels rowi)])
                                                    (if (equal? i build-pos-col)
                                                        (+ (get-space-level current-board build-pos-row build-pos-col) 1)
                                                        leveli))])
                             rowi))])))

(define (increment-turn current-board)
  (struct-copy board current-board
               [turn (+ (board-turn current-board) 1)]))


; Get the 8 spaces around a token. They may not be valid spaces to move or build.
; Returns as a list of pairs as such: ( (level . space1-coordinates) ... (level . space8-coordinates))
(define (get-8-spaces-levels board token-pos)
  (let* ([token-row (list-ref token-pos 0)]
         [token-col (list-ref token-pos 1)]
         [space1 (list (- token-row 1) (- token-col 1))]
         [space2 (list (- token-row 1) token-col)]
         [space3 (list (- token-row 1) (+ token-col 1))]
         [space4 (list token-row (- token-col 1))]
         [space5 (list token-row (+ token-col 1))]
         [space6 (list (+ token-row 1) (- token-col 1))]
         [space7 (list (+ token-row 1) token-col)]
         [space8 (list (+ token-row 1) (+ token-col 1))]
         [space1-level (get-space-level board (list-ref space1 0) (list-ref space1 1))]
         [space2-level (get-space-level board (list-ref space2 0) (list-ref space2 1))]
         [space3-level (get-space-level board (list-ref space3 0) (list-ref space3 1))]
         [space4-level (get-space-level board (list-ref space4 0) (list-ref space4 1))]
         [space5-level (get-space-level board (list-ref space5 0) (list-ref space5 1))]
         [space6-level (get-space-level board (list-ref space6 0) (list-ref space6 1))]
         [space7-level (get-space-level board (list-ref space7 0) (list-ref space7 1))]
         [space8-level (get-space-level board (list-ref space8 0) (list-ref space8 1))])
    (append (list (cons space1-level space1))
            (append (list (cons space2-level space2))
                    (append (list (cons space3-level space3))
                            (append (list (cons space4-level space4))
                                    (append (list (cons space5-level space5))
                                            (append (list (cons space6-level space6))
                                                    (append (list (cons space7-level space7))
                                                            (list (cons space8-level space8)))))))))))

; Returns a list of all the valid move spaces for a specific token
(define (get-valid-move-spaces board token-pos)
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
                       ; doesn't already contain other tokens and is not a level 4 space.
                       [(and (and (and (and (or (= level (+ token-level 1))
                                                (<= level token-level))
                                            (equal? (member space opponent-player-tokens) #f))
                                       (equal? (member space current-player-tokens) #f))
                                  (< level MAX-LEVEL))
                                  (> level INVALID-LEVEL)) #t]
                       [else #f])))
                 (get-8-spaces-levels board token-pos)))))

; Returns a list of all the valid build spaces for a specific token position.
; The token position is typically the new position a token is in after it moved.
(define (get-valid-build-spaces board token-pos)
  (let* ([token-level (get-token-level board token-pos)]
         [current-player-tokens (player-tokens (list-ref (board-players board) 0))]
         [opponent-player-tokens (player-tokens (list-ref (board-players board) 1))])
    (map (lambda (intermediate)
           (remove (first intermediate) intermediate)) ; remove the level value from the final list
         (filter (lambda (level-space)
                   (let * ([level (car level-space)]
                           [space (cdr level-space)])
                     (cond
                       ; Make sure the current build space is within the bounds of the token and
                       ; doesn't already contain other tokens and is not a level 4 space.
                       [(and (and (and (> level INVALID-LEVEL)
                                       (equal? (member space opponent-player-tokens) #f))
                                  (equal? (member space current-player-tokens) #f))
                             (< level MAX-LEVEL)) #t]
                       [else #f])))
                 (get-8-spaces-levels board token-pos)))))