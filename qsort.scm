#lang scheme
;; (split p lst)
;; Return a pair of mutually exclusive sublists (A . B) of lst.  A
;; contains those x in lst for which (p x) is true; B contains the
;; remaining elements of B.  The order of the original elements is
;; preserved.
;;
(define split
  (lambda (p lst)
    (cond ((null? lst)
	   '(() . ()))
	  (else
	   (let* ((rst (split p (cdr lst)))
		  (hd  (car rst))
		  (tl  (cdr rst))
		  (fst (car lst)))
	     (if (p fst)
		 (cons (cons fst hd) tl)
		 (cons hd (cons fst tl))))))))

;; (partition p lst)
;; Return a list of lists (A_1 ... A_n) such that 
;;   (apply append (A_1 ... A_n)) ==> lst
;; and for each i, any x, y in A_i have the property that
;; (p x) = (p y).
;;
(define partition
  (lambda (p lst)
    (cond ((null? lst) lst)
          (else
           (let ((rest (partition p (cdr lst))))
             (if (or (null? rest)
                     (not (equal? (p (car lst))
                                  (p (caar rest)))))
                 (cons (list (car lst)) rest)
                 (cons (cons (car lst) (car rest))
                       (cdr rest))))))))

;; (extract-ref lst pos)
;; Given an input list (a_1 ... a_n) and a position 1 <= i <= n,
;; return the list (a_i a_1 ... a_{i-1} a_{i+1} ... a_n).
;;
(define extract-ref
  (lambda (lst pos)
    (cond ((null? lst)
	   (error 'extract-ref "position ~a out of range" pos))
	  ((zero? pos)
	   lst)
	  (else
	   (let ((ex (extract-ref (cdr lst) (- pos 1))))
	     (cons (car ex)
		   (cons (car lst) (cdr ex))))))))

;; (insert lt? elt lst)
;; If lst is sorted, return a new list with elt inserted among the
;; elements of lst, using lt? as a comparison function.
;;
(define insert
  (lambda (lt? elt lst)
    (cond ((null? lst) (list elt))
	  ((lt? elt (car lst))
	   (cons elt lst))
	  (else
	   (cons (car lst)
		 (insert lt? elt (cdr lst)))))))

(define extract-random
  (lambda (lst)
    (let* ((pos (floor (* (random) (length lst)))))
      (extract-ref lst pos))))

(define choose-pivot extract-random)

(define qsort
  (lambda (lt? lst)
    (if (null? lst)
	lst
	(let* ((base  (choose-pivot lst))
	       (pivot (car base))
	       (part  (split (lambda (elt)
			       (lt? elt pivot))
			     (cdr base))))
	  (append (qsort lt? (car part))
		  (list pivot)
		  (qsort lt? (cdr part)))))))

(define foldr
  (lambda (fn zero lst)
    (cond ((null? lst) zero)
	  (else
	   (fn (car lst) (foldr fn zero (cdr lst)))))))

(define isort
  (lambda (lt? lst)
    (foldr (lambda (elt done)
	     (insert lt? elt done))
	   '()
	   lst)))

(define make-random-list
  (lambda (len lo hi)
    (if (zero? len)
	'()
	(cons (inexact->exact 
	       (+ lo (floor (* (random) 
			       (- (+ hi 1) lo)))))
	      (make-random-list (- len 1) lo hi)))))

(define is-sorted?
  (lambda (lt? lst)
    (or (null? lst)
	(null? (cdr lst))
	(and (lt? (car lst) (cadr lst))
	     (is-sorted? lt? (cdr lst))))))

(define L '(a b c d e))
(define M (make-random-list 200 1 2000))

(display "Sorting test, ") (display (length M)) (display " items.")
(newline)
(display "isort") (newline)
(let ((res (time (isort <= M)))) 
  (is-sorted? <= res))
(display "qsort") (newline)
(let ((res (time (qsort <= M)))) 
  (is-sorted? <= res))
