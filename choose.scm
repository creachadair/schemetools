;;;
;;; Name:     cp.scm
;;; Purpose:  A Scheme implementation of McCarthy's AMB operator.
;;; Author:   M. J. Fromberger <http://www.dartmouth.edu/~sting/>
;;; 

(define-syntax thunk
  (syntax-rules ()
    ((thunk e1 e2 ...)
     (lambda () e1 e2 ...))))

(define-syntax push!
  (syntax-rules ()
    ((push! elt lst)
     (set! lst (cons elt lst)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! lst)
     (let ((rtn (car lst)))
       (set! lst (cdr lst))
       rtn))))

;; A global list of choice points; the default element is a failure
;; node that aborts computation.
(define *cp*
  (let ((p (cons (thunk (error 'choose "no more choices")) #f)))
    (set-cdr! p p)
    p))

;; (choose *args)
;; If no args are available, back up to the last point at which a
;; choice was available and try a different path; otherwise,
;; nondeterministically choose a new path from among the args
;; presented.
(define choose
  (lambda args
    (cond ((null? args)
           ((pop! *cp*)))
          (else
           (call/cc (lambda (return)
                      (call/cc (lambda (cc)
                                 (push! (thunk (cc #f)) *cp*)
                                 (return (car args))))
                      (apply choose (cdr args))))))))

;; After a cut, all previous paths are discarded
(define (cut)
  (let loop ((cur *cp*))
    (cond ((not (eq? cur (cdr cur)))
           (pop! *cp*)
           (loop *cp*)))))

;; Here there be dragons

(define uniq
  (lambda (lst)
    (cond ((null? lst) lst)
          ((member (car lst) (cdr lst))
           (uniq (cdr lst)))
          (else
           (cons (car lst) (uniq (cdr lst)))))))

(define (solve-money)
  (let ((m 1)
        (n (choose 0 2 3 4 5 6 7 8 9))
        (o (choose 0 2 3 4 5 6 7 8 9))
        (r (choose 0 2 3 4 5 6 7 8 9))
        (s (choose 2 3 4 5 6 7 8 9))
        (y (choose 0 2 3 4 5 6 7 8 9))
        (e (choose 0 2 3 4 5 6 7 8 9))
        (d (choose 0 2 3 4 5 6 7 8 9)))
    (unless (or (= (+ d e) y) (= (+ d e) (+ y 10))) (choose))
    (unless (>= (+ s m 1) 10) (choose))
    
    (let ((send    (+ (* s 1000) (* e 100) (* n 10) d))
          (more    (+ (* m 1000) (* o 100) (* r 10) e))
          (money   (+ (* m 10000) (* o 1000) (* n 100) (* e 10) y)))
      (unless (= (+ send more) money) (choose))

      (let* ((soln (list s e n d m o r y))
             (okay (uniq soln)))
        (unless (= (length okay) (length soln)) (choose))
        soln))))
