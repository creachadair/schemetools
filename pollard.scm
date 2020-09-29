;;;
;;; Name:     pollard.scm
;;; Purpose:  Implements J. Pollard's "p - 1" factorization algorithm
;;; Author:   M. J. Fromberger <http://www.dartmouth.edu/~sting/>
;;; Info:     $Id: pollard.scm,v 1.1 2004/08/31 01:21:29 sting Exp $
;;;
;;; Notes:
;;; Written for Eli Barzilay's Swindle under PLT Scheme.
;;;

;; (exptmod b e m)
;; Compute b to the e power, modulo m (i.e., b^e (mod m))
(define exptmod
  (lambda ((b <integer>) (e <integer>) (m <integer>))
    (cond ((zero? e) 1)
          ((even? e)
           (modulo (square (exptmod b (quotient e 2) m)) m))
          (else
           (modulo (* b (exptmod b (- e 1) m)) m)))))

;; (square n)
(define square
  (lambda ((n <number>)) (* n n)))

;; ----- Problem 5 -----

;; J. M. Pollard's (p - 1) factorization method
;; (pollard n base max)
;;    n      - integer > 2 to be factored
;;    base   - base to use for exponentiation
;;    max    - number of values to try before giving up
;;
;; Returns #f if no factor was found within max candidates; otherwise,
;; returns a factor.

(define pollard
  (lambda ((n <integer>) (base <integer>) (max <integer>))
    (let loop ((r base) (k 1))
      (let* ((rnext (exptmod r k n))
	     (g (gcd (- rnext 1) n))) 
	(cond ((> k max) #f)           ; give up, we're tired of searching
	      ((or (= g 1) (= g n)) 
	       (loop rnext (+ k 1)))   ; nothing interesting, try the next one
	      (else 
	       g))))))                 ; found a factor!
;              (values k g)))))))      ; found a factor! (See note following)

;; Note:
;;
;;   An alternate implementation uses the 'values' procedure, which
;;   permits the return of multiple values.  I did this in order to
;;   return both the factor (g) and the value of k which gave us that
;;   factor.  Your solution can just return 'g' in this instance.  To
;;   play with the alternate implementation, comment out the second to
;;   last line of the solution, and uncomment the last line.

;; Solutions to sample outputs:
;; (pollard 39173679876983836729 3 3000)       ==> 6699424463
;; (pollard 2278570691794489592002651 2 27000) ==> 1274508504281

;; -- Some related interesting code

;; A tail-recursive version of 'exptmod' (the version provided in
;; ps2.scm is not tail recursive)

(define exptmod-tail
  (lambda ((b <integer>) (e <integer>) (m <integer>))
    (let loop ((s 1) (base b) (expt e))
      (if (zero? expt)
	  s
	  (let ((next-base (modulo (* base base) m))
		(next-expt (quotient expt 2)))
	    (cond ((odd? expt)
		   (loop (modulo (* s base) m) next-base next-expt))
		  (else
		   (loop s next-base next-expt))))))))

;; Another factoring algorithm, Fermat's method

;; Suppose n = ab.  We search for a way to write n as the difference
;; of two perfect squares.
;;
;; 1.  Choose t = floor(sqrt(n)).  
;; 2.  Let x = t^2 - n
;; 3.  If x is a perfect square, x = s^2 for some integer s.
;;     Thus, n = t^2 - s^2 = (t - s)(t + s), and we have two
;;     factors of n.
;; 4.  Otherwise, increment t and return to step 2.
;;
;; This is guaranteed to terminate eventually, although it will
;; perform best if you have factors which are close together.

(define (perfect-square? n)
  (let ((r (sqrt n)))
    (if (integer? r) r #f)))

(define (fermat n)
  (let loop ((t (ceiling (sqrt n))))
    (let* ((diff (- (* t t) n))
           (s (perfect-square? diff)))
      (if s
          (values (inexact->exact (+ t s)) 
                  (inexact->exact (- t s)))
          (loop (+ t 1))))))

;; Here there be dragons
