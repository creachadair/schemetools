;;
;; Gridlock Solver
;;
;; by Michael J. Fromberger <sting@cs.dartmouth.edu>
;; Copyright (C) 2003 Michael J. Fromberger, All Rights Reserved
;;
;; The game of Gridlock can be played online at:
;;   http://www.crystalnewmedia.com/gridlock/gridlock.html
;;
;; It's basically a sliding puzzle game, in which pieces of length
;; 2 or 3 are laid into a grid, with either horizontal or vertical
;; orientation.  The grid is 6 x 6, and I will assume the rows and
;; columns are labelled (0 .. 5).  In row 2, there is an exit at 
;; the right edge of the grid; everywhere else, the walls around
;; the grid are solid.
;;
;; The tiles slide only in their given orientation, and may not 
;; overlap.
;;

(load "search.scm")

(set! *table-size* 1009)

(define (make-tile dir length row col)
  (lambda (op)
    (case op
      ((dir direction) dir)
      ((len length) length)
      ((row) row)
      ((col) col)
      ((as-list) (list dir length row col))
      ((type) 'tile))))

(define (tile? obj)
  (and (procedure? obj)
       (eq? (obj 'type) 'tile)))

(define (tiles-eq? t1 t2)
  (equal? (t1 'as-list)
          (t2 'as-list)))

(define (tile->list tile)
  (tile 'as-list))

;; Given a tile, return all the positions it can be slid to in 
;; its row, assuming no other obstacles.  Omits the tile's 
;; current position fron the output list
;;
(define (slide-tile tile)
  (define (slide-tile-to tile pos)
    (case (tile 'dir)
      ((h) (make-tile 'h (tile 'len) (tile 'row) pos))
      ((v) (make-tile 'v (tile 'len) pos (tile 'col)))
      ))
  (let* ((dir (tile 'dir))
         (skip (if (eq? dir 'h) (tile 'col) (tile 'row)))
         (max  (- 6 (tile 'len))))

    (let loop ((i 0) (out '()))
      (cond ((> i max) out)
            ((= i skip) (loop (+ i 1) out))
            (else
             (loop (+ i 1) 
                   (cons (slide-tile-to tile i)
                         out))))
      )))

(define (tiles-overlap? t1 t2)
  (if (eq? (t1 'dir) (t2 'dir))
      (case (t1 'dir)
        ((h)
         (and (= (t1 'row) (t2 'row))
              (or (<= (t1 'col) (t2 'col) (+ (t1 'col) (t1 'len) -1))
                  (<= (t2 'col) (t1 'col) (+ (t2 'col) (t2 'len) -1)))))
        ((v)
         (and (= (t1 'col) (t2 'col))
              (or (<= (t1 'row) (t2 'row) (+ (t1 'row) (t1 'len) -1))
                  (<= (t2 'row) (t1 'row) (+ (t2 'row) (t2 'len) -1))))))
      (case (t1 'dir)
        ((h)
         (and (<= (t1 'col) (t2 'col) (+ (t1 'col) (t1 'len) -1))
              (<= (t2 'row) (t1 'row) (+ (t2 'row) (t2 'len) -1))))
         
        ((v)
         (and (<= (t2 'col) (t1 'col) (+ (t2 'col) (t2 'len) -1))
              (<= (t1 'row) (t2 'row) (+ (t1 'row) (t1 'len) -1))))
        )))

(define (point-in-tile? row col tile)
  (case (tile 'dir)
    ((h)
     (and (= row (tile 'row))
          (<= (tile 'col) col (+ (tile 'col) (tile 'len) -1))))
    ((v)
     (and (= col (tile 'col))
          (<= (tile 'row) row (+ (tile 'row) (tile 'len) -1))))))

;; A board is specified by its tiles.  The first tile is assumed
;; to be the "key" tile (the one which needs to escape)
;;
(define (make-board tiles)
  (let* ((tlist (map (lambda (tile)
                       (if (tile? tile)
                           tile
                           (apply make-tile tile)))
                     tiles))
         (key-tile (car tlist)))
    (lambda (op . args)
      (case op
        ((tiles) tlist)
        ((key-tile) (car tlist))
        ((type) 'board)))
    ))

(define (board? obj)
  (and (procedure? obj)
       (eq? (obj 'type) 'board)))

(define (boards-eq? b1 b2)
  (let loop ((t1 (b1 'tiles))
             (t2 (b2 'tiles)))
    (cond ((and (null? t1) (null? t2)) #t)
          ((or  (null? t1) (null? t2)) #f)
          ((tiles-eq? (car t1) (car t2))
           (loop (cdr t1) (cdr t2)))
          (else #f)
          )))
    
;; Make a copy of the given board, in which the given old tile
;; is replaced by the given new tile.
;;
(define (copy-board brd old new)
  (define (replace-tile old new lst)
    (cond ((null? lst) lst)
          ((tiles-eq? old (car lst))
           (cons new (cdr lst)))
          (else
           (cons (car lst) (replace-tile old new (cdr lst))))
          ))
  (let ((old-tile (if (tile? old) old (apply make-tile old)))
        (new-tile (if (tile? new) new (apply make-tile new))))
    (make-board (replace-tile old-tile new-tile (brd 'tiles)))
    ))

;; Return a list of all the boards reachable by one move from the
;; given board (not including the null move)
;;
(define (reachable-boards board)
  (define (valid-slides tile)
    (let* ((slides (slide-tile tile))
           (rest   (remove (lambda (t) (tiles-eq? t tile))
                           (board 'tiles)))
           (tboard (make-board rest)))

      (filter (lambda (t1)
                (and 
                 (no (lambda (t2)
                       (tiles-overlap? t1 t2))
                     rest)
                 (clear-path? tboard tile t1)))
              slides)
      ))
  
  (foldr append '()
         (map (lambda (old)
                (map (lambda (new)
                       (copy-board board old new))
                     (valid-slides old)))
              (board 'tiles))))

;; Return a list of (pos old new) where pos is the position of the
;; tile to be moved in the tiles list, old and new are the offsets
;; of the old and new positions.  Old and new are rows for vertical
;; tiles, columns for horizontal tiles.
;;
(define (compare-boards b1 b2)
  (let loop ((t1 (b1 'tiles))
             (t2 (b2 'tiles))
             (pos 0))
    (cond ((or (null? t1) (null? t2)) #f)
          ((tiles-eq? (car t1) (car t2))
           (loop (cdr t1) (cdr t2) (+ pos 1)))
          (else
           (case ((car t1) 'dir)
             ((h) (list pos ((car t1) 'col) ((car t2) 'col)))
             ((v) (list pos ((car t1) 'row) ((car t2) 'row)))))
          )))

(define (board->list board)
  (map (lambda (t) (t 'as-list)) (board 'tiles)))

;; Display a textual representation of the given board
(define (print-board board)
  (define (rc-pos row col) (+ (* row 6) col))
  (let ((tiles (board 'tiles))
        (template (make-vector 36 0)))
    
    (let next-tile ((tiles tiles) (counter 1))
      (if (null? tiles)
          (do ((row 0 (+ row 1)))
            ((> row 5))
            (do ((col 0 (+ col 1)))
              ((> col 5) (newline))
              (display 
               (number->string 
                (vector-ref template (rc-pos row col)) 16))
              ))
          
          (let* ((tile (car tiles))
                 (trow (tile 'row))
                 (tcol (tile 'col))
                 (tcend (+ tcol (tile 'len) -1))
                 (trend (+ trow (tile 'len) -1)))
            
            (case (tile 'dir)
              ((h)
               (do ((col tcol (+ col 1)))
                 ((> col tcend))
                 (vector-set! template (rc-pos trow col) counter)))
              ((v)
               (do ((row trow (+ row 1)))
                 ((> row trend))
                 (vector-set! template (rc-pos row tcol) counter))))
              (next-tile (cdr tiles) (+ counter 1))
            )))))

(define (cell-blank? board row col)
  (no (lambda (tile)
        (point-in-tile? row col tile))
      (board 'tiles)))

(define (clear-path? board orig new)
  (case (orig 'dir)
    ((h)
     (let ((from (min (orig 'col) (new 'col)))
           (to   (max (orig 'col) (new 'col)))
           (row  (orig 'row)))
       (let loop ((cur from))
         (cond ((> cur to) #t)
               ((cell-blank? board row cur)
                (loop (+ cur 1)))
               (else #f)))))
    
    ((v)
     (let ((from (min (orig 'row) (new 'row)))
           (to   (max (orig 'row) (new 'row)))
           (col  (orig 'col)))
       (let loop ((cur from))
         (cond ((> cur to) #t)
               ((cell-blank? board cur col)
                (loop (+ cur 1)))
               (else #f)))))
    ))
  
(define (key-free? board)
  (let* ((key-tile (board 'key-tile))
         (right-edge (+ (key-tile 'col) (key-tile 'len)))
         (tile-row (key-tile 'row)))
    (let loop ((i right-edge))
      (if (> i 5)
          #t
          (and (cell-blank? board tile-row i)
               (loop (+ i 1)))))
    ))

(define (filter pred? lst)
  (cond ((null? lst) lst)
        ((pred? (car lst))
         (cons (car lst) 
               (filter pred? (cdr lst))))
        (else
         (filter pred? (cdr lst)))
        ))

(define (no pred? lst)
  (cond ((null? lst) #t)
        ((pred? (car lst)) #f)
        (else
         (no pred? (cdr lst)))))

(define (remove pred? lst)
  (cond ((null? lst) lst)
        ((pred? (car lst))
         (cdr lst))
        (else
         (cons (car lst)
               (remove pred? (cdr lst))))
        ))

(define (foldr fn init lst)
  (cond ((null? lst) init)
        (else
         (fn (car lst)
             (foldr fn init (cdr lst))))
        ))

(define (print-solution soln)
  (cond ((null? soln) 
         (display "<invalid>")
         (newline))
        ((null? (cdr soln))
         (display "Free the key block.")
         (newline))
        (else
         (let* ((diff (compare-boards (car soln)
                                      (cadr soln)))
                (tile (list-ref ((car soln) 'tiles)
                                (car diff)))
                (dirn (if (eq? (tile 'dir) 'h)
                          "horizontal"
                          "vertical"))
                (move (if (eq? (tile 'dir) 'h) "column" "row")))
           (display "Move the ")
           (display dirn) (display " tile at (")
           (display (tile 'row)) (display ", ") (display (tile 'col))
           (display ") from ")
           (display move) (display " ") (display (cadr diff))
           (display " to ")
           (display move) (display " ") (display (caddr diff))
           (newline)
           (print-solution (cdr soln))))
        ))

(define (board-hash brd)
  (let loop ((tiles (brd 'tiles)) (pos 1) (hash 0))
    (cond ((null? tiles) hash)
          (else
           (loop (cdr tiles)
                 (+ pos 1)
                 (+ hash (* (tile-hash (car tiles)) pos)))))))
    
(define (tile-hash tile)
  (let ((lst (tile 'as-list)))
    (+ (symbol-hash (car lst))
       (* 3 (list-ref lst 1))
       (* 5 (list-ref lst 2))
       (* 7 (list-ref lst 3)))))

(define solver (make-search boards-eq? board-hash
                            key-free? 
                            reachable-boards 
                            breadth-first))

(load "boards.scm")
