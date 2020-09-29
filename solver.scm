;;
;; solver.scm
;;

(define (rate m)
  (case m
    ((claudette) 1)
    ((maj-johnson) 2)
    ((cap-kangaroo) 5)
    ((col-mustard) 10)))

(define (cost group)
  (apply max (map rate group)))

(define (make-config cost pos left right)
  (list cost pos left right))
(define (config-cost c) (list-ref c 0))
(define (config-pos c) (list-ref c 1))
(define (config-left c) (list-ref c 2))
(define (config-right c) (list-ref c 3))
(define (config? obj) 
  (and (list? obj) (= (length obj) 4)))
(define (config-eq? c1 c2)
  (equal? (cdr c1) (cdr c2)))
(define (expand-config c)
  (let* ((old-pos (config-pos c))
         (old-cost (config-cost c))
         
         (from    (if (eq? old-pos 'left)
                      (config-left c)
                      (config-right c)))
         
         (to      (if (eq? old-pos 'left)
                      (config-right c)
                      (config-left c)))
         
         (moves   (append (k-subsets from 1)
                          (k-subsets from 2)))
         
         (new-from 
          (map (lambda (move)
                 (set-diff from move))
               moves))
        
         (new-to    (map (lambda (move)
                           (set-union to move))
                         moves))
         
         (new-cost  (map (lambda (move)
                           (+ old-cost (cost move)))
                         moves)))

    
    (case old-pos
      ((left)
       (map (lambda (cost left right)
              (make-config cost 'right left right))
            new-cost new-from new-to))
      ((right)
       (map (lambda (cost left right)
              (make-config cost 'left left right))
            new-cost new-to new-from))
      )))

(define (set-union L1 L2)
  (append L1 (set-diff L2 L1)))

(define (set-diff L1 L2)
  (filter (lambda (obj)
            (not (memq obj L2)))
          L1))

(define (make-search state-eq? goal? expand next-queue)
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
    (let loop ((queue (list (list start))) ;; Queue of unexplored states
               (seen (list start)))        ;; Set of previously observed states
      (cond ((null? queue)
             #f)
            ((goal? (caar queue))
             (car queue))
            (else
             (let* ((next (set-diff (expand (caar queue)) seen))
                    (succ (map (lambda (s) (cons s (car queue))) next))
                    (new-seen (set-union seen next)))
               (loop (next-queue (caar queue) (cdr queue) succ) new-seen)))
            ))))

(define (breadth-first prev oldq succ)
  (append oldq succ))


(define (k-subsets lst k)
  (cond ((zero? k)
         '(()))
        ((null? lst)
         '())
        ((= k (length lst))
         (list lst))
        (else
         (let ((incl (map (lambda (e)
                            (cons (car lst) e))
                          (k-subsets (cdr lst) (- k 1))))
               (excl (k-subsets (cdr lst) k)))
           (append incl excl)
           ))))

;; (filter keep? L) 
;; Return a list of all elements x in L for which (keep? x) is true.
(define (filter keep? L)
  (cond ((null? L) L)
        ((keep? (car L))
         (cons (car L) (filter keep? (cdr L))))
        (else
         (filter keep? (cdr L)))))

(define (make-sorter before?)
  (define (insert obj L)
    (cond ((or (null? L)
               (before? obj (car L)))
           (cons obj L))
          ((before? (car L) obj)
           (cons (car L) (insert obj (cdr L))))
          (else
           (cons obj L))
          ))
  (lambda (lst)
    (let loop ((rest lst) (out '()))
      (cond ((null? rest) out)
            (else
             (loop (cdr rest)
                   (insert (car rest) out)))
            ))))

(define (solution? state)
  (and (eq? (config-pos state) 'right)
       (= (length (config-right state)) 4)))

(define order-queue
  (let ((sort (make-sorter (lambda (c1 c2)
                             (<= (config-cost (car c1))
                                 (config-cost (car c2)))))))
    (lambda (prev oldq succ)
      (sort (append oldq succ)))))

(define solve-problem
  (make-search config-eq? solution? expand-config order-queue))

(define *start-config*
  (make-config 0 'left '(claudette maj-johnson cap-kangaroo col-mustard) '()))
