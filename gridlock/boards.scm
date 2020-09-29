;; (make-board TILE-LIST)
;; The first tile is the "key" tile (the one that has to be made free).
;; The tile format is (DIR LEN ROW COL)
;; DIR is (h)orizontal or (v)ertical.
;; LEN is length in blocks.
;; ROW is the leftmost/topmost row (0-indexed)
;; COL is the leftmost/topmost col (0-indexed)

(define board2
  (make-board '((h 2 2 1)
		(h 2 3 1) (v 3 2 3) (v 3 3 5)
		(v 2 4 1) (h 2 5 2))))

(define board25
  (make-board '((h 2 2 1)
                (h 2 0 0) (v 2 0 2) (h 2 0 4)
                (h 2 1 0) (v 3 1 5) (v 3 2 0) (v 2 2 4)
                (h 3 3 1) (v 2 4 1) (v 2 4 3) (h 2 4 4)
                (h 2 5 4))))

(define board26
  (make-board '((h 2 2 1)
                (v 2 0 1) (h 3 0 3)
                (v 2 1 0) (v 2 1 3) (v 3 1 4)
                (v 2 2 5)
                (v 2 3 0) (h 3 3 1)
                (v 2 4 2) (v 2 4 5)
                (h 2 5 3))))

(define board27
  (make-board '((h 2 2 0) 
		(v 2 0 0) (h 2 0 1) (h 2 1 1) (v 3 0 3) (v 2 2 2) 
		(v 3 2 5) (h 2 3 3) (v 2 4 2) (h 3 5 3))))
	      

(define board29
  (make-board '((h 2 2 0) 
		(h 3 0 0) (v 3 0 4) (v 2 1 2) (v 2 2 5) (v 2 3 0) 
		(h 2 3 1) (h 2 3 3) (h 2 4 1) (v 2 4 3) (v 2 4 5) 
		(h 3 5 0))))
	      
(define board31
  (make-board '((h 2 2 1)
		(h 2 0 0) (h 3 0 3) (v 2 1 3) (h 2 1 4)
		(v 2 2 0) (v 3 2 5) (v 3 3 2) (h 2 3 3)
		(h 2 4 0) (h 3 5 3))))

(define board32
  (make-board '((h 2 2 0)
		(h 2 0 0) (v 3 0 2) (v 2 0 3) (h 2 0 4)
		(v 2 3 0) (h 2 3 1) (h 2 3 3) (v 3 3 5)
		(v 2 4 3) (h 2 5 0))))

(define board33
  (make-board '((h 2 2 0)
		(v 2 0 1) (v 3 0 2) (h 2 0 4) (v 2 3 0)
		(h 2 3 1) (h 2 4 1) (h 2 3 3) (v 3 3 5)
		(v 2 4 3) (v 2 4 4) (h 3 5 0))))

(define board34
  (make-board '((h 2 2 0)
		(v 2 0 0) (h 3 0 3) (v 2 1 3) (v 3 1 5)
		(v 2 2 4) (h 3 3 0) (v 2 3 3) (v 2 4 2)
		(h 2 4 4) (h 2 5 0) (h 2 5 3))))

(define board35
  (make-board '((h 2 2 0)
		(v 3 0 2) (h 2 0 3) (v 3 0 5) (v 2 1 3)
		(v 2 3 0) (h 3 3 1) (h 2 4 1) (v 2 4 3)
		(v 2 4 4) (h 2 5 0))))

(define board36
  (make-board '((h 2 2 2)
		(v 3 0 0) (h 3 0 1) (h 2 0 4) (v 2 1 1)
		(h 2 1 2) (v 3 1 5) (h 3 3 0) (v 2 3 3)
		(v 2 4 2) (h 2 4 4) (h 2 5 0))))

(define board37
  (make-board '((h 2 2 1)
		(h 2 0 0) (v 2 0 2) (h 2 0 4) (h 2 1 0)
		(v 3 1 4) (v 3 1 5) (v 3 2 0) (h 3 3 1)
		(v 2 4 3) (h 2 4 4) (h 2 5 0) (h 2 5 4))))

(define board38
  (make-board '((h 2 2 0)
		(v 2 0 0) (h 3 0 3) (h 2 1 1) (v 2 1 3)
		(v 2 2 2) (v 3 2 5) (h 2 3 3) (v 2 4 2)
		(h 2 4 3) (h 3 5 3))))

(define board39
  (make-board '((h 2 2 0)
		(v 2 0 2) (h 3 0 3) (v 2 1 3) (v 2 2 2)
		(v 3 2 5) (h 2 3 0) (h 2 3 3) (v 2 4 0)
		(v 2 4 1) (h 2 4 2) (h 2 5 2))))

(define board40
  (make-board '((h 2 2 3)
		(v 3 0 0) (h 2 0 1) (v 2 0 4) (v 2 1 1)
		(v 2 1 2) (v 3 1 5) (h 3 3 0) (v 2 3 3)
		(v 2 4 2) (h 2 4 4) (h 2 5 0) (h 2 5 3))))
