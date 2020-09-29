(define *table-size* 131)

(define (make-table equal? hash size)
  (define (find-key key lst)
    (cond ((null? lst) #f)
          ((equal? key (caar lst))
           (car lst))
          (else
           (find-key key (cdr lst)))))
  (let ((table (make-vector (if (zero? size)
                                *table-size* size) '())))
    (lambda (op . args)
      (case op
        ((lookup)
         (let* ((key (car args))
                (ix  (modulo (hash key) (vector-length table))))
           (find-key key (vector-ref table ix))
           ))
        ((insert)
         (let* ((key (car args))
                (val (cadr args))
                (ix  (modulo (hash key) (vector-length table)))
                (fnd (find-key key (vector-ref table ix))))
           (if fnd
               (set-cdr! fnd val) 
               (vector-set! table ix
                            (cons (cons key val)
                                  (vector-ref table ix))))))
        ((table) table)
        ))))

(define (string-hash str)
  (let loop ((pos 1) (hash 0) (chars (string->list str)))
    (cond ((null? chars) hash)
          (else
           (loop (+ pos 1)
                 (modulo (+ hash (* pos (char->integer (car chars))))
                         4164737977)
                 (cdr chars)))
          )))
(define (symbol-hash sym)
  (string-hash (symbol->string sym)))

(define (make-search state-eq? state-hash goal? expand next-queue)
  (define (set-member elt S)
    (cond ((null? S) #f)
          ((state-eq? elt (car S)) #t)
          (else (set-member elt (cdr S)))))
  (define (set-diff S1 S2)
    (filter (lambda (obj)
              (not (set-member obj S2))) S1))
  (define (set-union S1 S2)
    (append S1 (set-diff S2 S1)))
  (lambda (start)
    (let ((seen (make-table state-eq? state-hash 0)))  
      
      (let loop ((queue (list (list start))))  ;; Queue of unexplored states

        (cond ((null? queue)
               #f)
              
              ((goal? (caar queue))
               (car queue))
              
              (else
               (let* ((next (filter (lambda (st)
                                      (not (seen 'lookup st)))
                                    (expand (caar queue))))
                      (succ (map (lambda (s)
                                   (cons s (car queue)))
                                 next)))
                 
                 (for-each (lambda (st)
                             (seen 'insert st #f))
                           next)
                 (loop (next-queue (caar queue) (cdr queue) succ)))))
              ))))

(define (breadth-first prev oldq succ)
  (append oldq succ))

(define (depth-first prev oldq succ)
  (append succ oldq))
