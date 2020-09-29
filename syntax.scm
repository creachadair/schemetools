#lang scheme
(define check-structure
  (lambda (obj pattern)
    (printf "object  = ~a~npattern = ~a~n~n" obj pattern)
    (if (pair? pattern)
        (case (car pattern)
          ((or) 
           (some (lambda (pat)
                   (check-structure obj pat))
                 (cdr pattern)))
          
          ((list)
           (and (list? obj)
                (every (lambda (elt)
                         (check-structure elt (cadr pattern)))
                       obj)))

          (else
           (and (pair? obj)
                (check-structure (car obj) (car pattern))
                (check-structure (cdr obj) (cdr pattern)))))
        
        (case pattern
          ((:id :symbol :name)   (symbol? obj))
          ((:num)   (number? obj))
          ((:int)   (integer? obj))
          ((:index) (and (integer? obj) (>= obj 0)))
          ((:list)  (list? obj))
          ((:any)   #t)
          (else     (equal? obj pattern)))
        )))
          
(define let-transform
  (lambda (form)
    (if (symbol? (cadr form))
        (let* ((loop   (cadr form))
               (binds  (caddr form))
               (names  (map car binds))
               (exprs  (map cadr binds))
               (body   (cdddr form)))
          `(letrec ((,loop (lambda (,@names)
                             ,@body)))
             (,loop ,@exprs)))
        
        (let* ((binds  (cadr form))
               (names  (map car binds))
               (exprs  (map cadr binds))
               (body   (cddr form)))
          `((lambda (,@names)
              ,@body) ,@exprs)))
    ))
     
(define lambda-syntax
  '(lambda (or :id
               (list :id))
     . :any))
(define let-syntax
  '(or (let (list (:id :any)) . :any)
       (let :id (list (:id :any)) . :any)))

