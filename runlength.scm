;;;
;;; runlength.scm
;;;
;;; An implementation of run-length coding for lists
;;; The interface consists of:
;;; (list->runlist lst)  -- returns a list of (obj . count) pairs
;;; (encode-runlist rl)  -- encode a runlist using duplication coding
;;; (decode-runlist rle) -- decode a duplication-coded runlist
;;; (runlist->list rl)   -- inflates a runlist into its equivalent long list

(define list->runlist
  (lambda (lst)
    (if (null? lst) '()
        (let loop ((cur (car lst))
                   (num 1)
                   (out '())
                   (rest (cdr lst)))
          
          (cond ((null? rest)
                 (reverse (cons
                           (if (= num 1)
                               cur
                               (cons cur num))
                           out)))
                
                ((equal? (car rest) cur)
                 (loop cur (+ num 1) out (cdr rest)))
                
                (else
                 (let ((blob (if (= num 1)
                                 cur
                                 (cons cur num))))
                   (loop (car rest) 
                         1 
                         (cons blob out)
                         (cdr rest)))))
                ))))

(define encode-runlist
  (lambda (runlst)
    (let loop ((out '()) (rest runlst))
      (cond ((null? rest)
             (reverse out))
            
            ((not (pair? (car rest)))
             (loop (cons (car rest) out) (cdr rest)))
            
            (else
             (loop (append (list 
                            (- (cdar rest) 2)
                            (caar rest)
                            (caar rest)) out)
                   (cdr rest))))
            )))

(define decode-runlist
  (lambda (lst)
    (cond ((null? lst) '())
          ((null? (cdr lst))
           (list (cons (car lst) 1)))
          (else 
           (let loop ((out '()) (rest lst))
             (cond ((null? rest) 
                    (reverse out))
                   
                   ((equal? (car rest) (cadr rest))
                    (loop (cons (cons (car rest) (+ (caddr rest) 2))
                                out)
                          (cdddr rest)))
                   
                   (else
                    (loop (cons (car rest) out) (cdr rest))))
                   )))))

(define runlist->list
  (letrec ((make-k-list
            (lambda (obj k)
              (let loop ((out '()) (num k))
                (if (zero? num)
                    out
                    (loop (cons obj out) (- num 1)))))))
    (lambda (runlst)
      (cond ((null? runlst) '())
            
            (else
             (let loop ((out '()) (rest runlst))
               (cond ((null? rest)
                      (reverse out))
                     
                     ((pair? (car rest))
                      (loop (append (make-k-list (caar rest) (cdar rest))
                                    out)
                            (cdr rest)))
                     
                     (else
                      (loop (cons (car rest) out) (cdr rest)))
                     )))
            ))))

(define compress
  (lambda (lst)
    (encode-runlist (list->runlist lst))))

(define decompress
  (lambda (lst)
    (runlist->list (decode-runlist lst))))

(define L '(a a a a a a b b c e e e e e e e e b c c c c c c c a a f f f f f))
