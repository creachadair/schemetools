(define-syntax p-let
  (syntax-rules (= :)
    ((p-let () body ...)
     (let () body ...))

    ((p-let (((x : p) expr) more ...) body ...)
     (let* ((e expr) (x (car expr)))
       (p-let ((p (cdr e)) more ...) body ...)))

    ((p-let (((x) expr) more ...) body ...)
     (let ((x (car expr)))
       (p-let (more ...) body ...)))

    ((p-let ((x = p expr) more ...) body ...)
     (let ((x expr))
       (p-let ((p x) more ...) body ...)))

    ((p-let ((x : p expr) more ...) body ...)
     (p-let (((x : p) expr) more ...) body ...))

    ((p-let ((x : y : 
    
    ((p-let ((x : p : pat ... expr) more ...) body ...)
     (p-let ((x : (p : pat ...) expr) more ...) body ...))
     
    ((p-let ((x : p expr) more ...) body ...)
     (let* ((e expr) (x (car e)))
       (p-let ((p (cdr e)) more ...) body ...)))
    
    ((p-let (((x : p) expr) more ...) body ...)
     (p-let ((x : p expr) more ...) body ...))
    
    ((p-let ((x = p expr) more ...) body ...)
     (let* ((x expr))
       (p-let ((p x) more ...) body ...)))
    
    ((p-let (((x) expr) more ...) body ...)
     (let* ((x (car expr)))
       (p-let (more ...) body ...)))
    
    ((p-let ((p expr) more ...) body ...)
     (let ((p expr))
       (p-let (more ...) body ...)))))

(define third
  (lambda (lst)
    (p-let ((_ : (_ : (th : _)) lst))
      th)))
