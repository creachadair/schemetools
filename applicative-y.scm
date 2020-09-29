#lang scheme

;; This fails
;((lambda (lst)
;   (cond ((null? lst) 0)
;         (else
;          (add1 (length (cdr lst))))))
; '(1 2 3 4 5))

((lambda (fn lst)
   (cond ((null? lst) 0)
         (else
          (add1 (fn  fn (cdr lst))))))
 (lambda (fn lst)
   (cond ((null? lst) 0)
         (else
          (add1 (fn fn (cdr lst))))))
 '(1 2 3 4 5))

(((lambda (fn)
    (lambda (lst)
      (cond ((null? lst) 0)
            (else
             (add1 ((fn fn) (cdr lst)))))))
  (lambda (fn)
    (lambda (lst)
      (cond ((null? lst) 0)
            (else
             (add1 ((fn fn) (cdr lst))))))))
 '(1 2 3 4 5))

((lambda (fn)
   (lambda (lst)
     (cond ((null? lst) 0)
           (else 
            (add1 ((lambda (x) ((fn fn) x)) (cdr lst)))))))
 (lambda (fn)
   (lambda (lst)
     (cond ((null? lst) 0)
           (else 
            (add1 ((lambda (x) ((fn fn) x)) (cdr lst))))))))

((lambda (fn)
   (let ((r (lambda (x) ((fn fn) x))))
     (lambda (lst)
       (cond ((null? lst) 0)
             (else
              (add1 (r (cdr lst))))))))
 (lambda (fn)
   (let ((r (lambda (x) ((fn fn) x))))
     (lambda (lst)
       (cond ((null? lst) 0)
             (else
              (add1 (r (cdr lst)))))))))

((lambda (fn)
   ((lambda (r)
      (lambda (lst)
        (cond ((null? lst) 0)
              (else
               (add1 (r (cdr lst)))))))
    (lambda (x) ((fn fn) x))))
 (lambda (fn)
   ((lambda (r)
      (lambda (lst)
        (cond ((null? lst) 0)
              (else
               (add1 (r (cdr lst)))))))
    (lambda (x) ((fn fn) x)))))

(let ((m (lambda (r)
           (lambda (lst)
             (cond ((null? lst) 0)
                   (else
                    (add1 (r (cdr lst)))))))))
  ((lambda (fn)
     (m (lambda (x) ((fn fn) x))))
   (lambda (fn)
     (m (lambda (x) ((fn fn) x))))))

((lambda (m)
   ((lambda (fn)
      (m (lambda (x) ((fn fn) x))))
    (lambda (fn)
      (m (lambda (x) ((fn fn) x))))))
 (lambda (r)
   (lambda (lst)
     (cond ((null? lst) 0)
           (else
            (add1 (r (cdr lst))))))))

(define Y
  (lambda (m)
    ((lambda (fn)
       (m (lambda (x) ((fn fn) x))))
     (lambda (fn)
       (m (lambda (x) ((fn fn) x)))))))

(define lenf
  (lambda (r)
    (lambda (lst)
      (cond ((null? lst) 0)
            (else
             (add1 (r (cdr lst))))))))

(define length (Y lenf))
