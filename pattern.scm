#lang scheme 
;;;
;;; Name:    pattern.scm
;;; Purpose: Pattern matching and substitution.
;;; Author:  M. J. Fromberger <http://www.dartmouth.edu/~sting/>
;;; Info:    $Id$
;;;

(define any
  (lambda (x) x))

(define-syntax rule-set
  (syntax-rules ()
    ((rule-set ((type? var1 var2 ...) ...)
               (pattern template) ...)
     (let ((rules (quote ((pattern . template) ...))))
       (letrec ((is-pvar?
                 (lambda (pat)
                   (case pat
                     ((var1 var2 ...) type?)
                     ...
                     (else #f))))
                
                (is-eval?
                 (lambda (obj)
                   (and (list? obj)
                        (not (null? obj))
                        (eq? (car obj) 'eval)
                        (not (null? (cdr obj))))))
                
                (rewrite-expr
                 (lambda (expr)
                   (try-rules (if (pair? expr)
                                  (cons (rewrite-expr (car expr))
                                        (rewrite-expr (cdr expr)))
                                  expr))))
                
                (matches
                 (lambda (expr pat dict)
                   (cond ((not dict) #f)

                         ((is-pvar? pat) =>
                          (lambda (type-matches?)
                            (and (type-matches? expr)
                                 (extend pat expr dict))))
                         
                         ((pair? pat)
                          (and (pair? expr)
                               (matches (cdr expr)
                                        (cdr pat)
                                        (matches (car expr)
                                                 (car pat)
                                                 dict))))

                         ((vector? pat)
                          (and (vector? expr)
                               (= (vector-length pat)
                                  (vector-length expr))
                               (matches (vector->list expr)
                                        (vector->list pat)
                                        dict)))
                         
                         (else
                          (and (equal? expr pat) dict)))))

                (evaluate
                 (lambda (obj dict)
                   (if (eq? obj 'eval) obj
                       (let ((thing (fill-in obj dict)))
                         (eval thing (interaction-environment))))))
                
                (fill-in
                 (lambda (tmpl dict)
                   (let ((found (assq tmpl dict)))
                     (if found (cdr found)
                         (cond 
                          ((is-eval? tmpl)
                           (evaluate (cadr tmpl) dict))
                          
                          ((pair? tmpl)
                           (cons (fill-in (car tmpl) dict)
                                 (fill-in (cdr tmpl) dict)))
                          
                          ((vector? tmpl)
                           (list->vector
                            (map (lambda (elt)
                                   (fill-in elt dict))
                                 (vector->list tmpl))))
                          
                          (else tmpl))))))
                
                (extend
                 (lambda (key val dict)
                   (let ((found (assq key dict)))
                     (if found
                         (and (equal? (cdr found) val) dict)
                         (cons (cons key val) dict)))))
                
                (try-rules
                 (lambda (expr)
                   (let next ((rules rules))
                     (cond ((null? rules)
                            expr)
                           (else
                            (let* ((pat  (caar rules))
                                   (tmpl (cdar rules))
                                   (hit  (matches expr pat '())))
                              (cond (hit
                                     (rewrite-expr (fill-in tmpl hit)))
                                    (else
                                     (next (cdr rules)))))))))))
         (lambda (expr)
           (rewrite-expr expr)))))))

(define member-of
  (lambda (lst)
    (lambda (obj)
      (memq obj lst))))

(define simplify
  (rule-set ((any
              e e1 e2)
             (number?
              c c1 c2)
             (symbol?
              v)
             ((member-of '(+ - * /))
              op op1 op2)
             ((member-of '(+ -))
              aop aop1 aop2))
            
    ( (op c1 c2)          (eval (op c1 c2)) )
    ( (^ c1 c2)           (eval (expt c1 c2)) )
    ( (+ 0 e)             e )
    ( (* 1 e)             e )
    ( (* 0 e)             0 )
    ( (^ e 0)             1 )
    ( (^ e 1)             e )
    ( (+ e c)             (+ c e) )
    ( (* e c)             (* c e) )
    ( (- e e)             0 )
    ( (+ e e)             (* 2 e) )
    ( (* e e)             (^ e 2) )

    ( (aop c1 (aop c2 e))   (+ (aop c1 c2) e) )
    ( (aop1 c1 (aop2 c2 e)) (- (aop1 c1 c2) e) )
    ( (* c1 (* c2 e))       (* (* c1 c2) e) )

    ( (* (* e1 e2) e)     (* e1 (* e2 e)) )
    ( (+ (+ e1 e2) e)     (+ e1 (+ e2 e)) )

    ( (* e1 (* c e2))     (* c (* e1 e2)) )
    ( (+ e1 (+ c e2))     (+ c (+ e1 e2)) )
    ( (+ (* c e) e)       (* (eval (+ c 1)) e) )
    ( (- (* c e) e)       (* (eval (- c 1)) e) )

    ( (+ v (op c e))      (+ (op c e) v) )
    ( (* v (op c e))      (* (op c e) v) )

    ( (+ (* c1 e) (* c2 e)) (* (+ c1 c2) e) )))

(define deriv
  (rule-set ((any u v)
             (number? c)
             (symbol? x y))
    ( (dd c x)         0 )
    ( (dd x x)         1 )
    ( (dd y x)         0 )
    ( (dd (+ u v) x)   (+ (dd u x) (dd v x)) )
    ( (dd (* u v) x)   (+ (* u (dd v x))
                          (* v (dd u x))) )
    ( (dd (^ u c) x)   (* (* c (^ u (eval (- c 1))))
                          (dd u x)) )))

(define latexify
  (lambda (expr)
    (cond ((null? expr) "")
          ((list? expr)
           (let ((lc (latexify (cadr expr)))
                 (rc (latexify (caddr expr))))
             (case (car expr)
               ((dd)
                (string-append "\\frac{d}{d" rc "}" lc))
               ((+ -)
                (string-append lc " + " rc))
               ((*)
                (string-append lc " \\cdot " rc))
               ((/)
                (string-append "\\frac{" lc "}"
                               "{" rc "}"))
               ((^)
                (string-append "{" lc "}^{" rc "}"))
               (else
                "\\mbox{unknown operator}"))))
          ((symbol? expr)
           (let ((name (symbol->string expr)))
             (if (= (string-length name) 1)
                 name
                 (string-append "\\mathit{" name "}"))))
          ((number? expr)
           (number->string expr))
          ((string? expr)
           expr)
          (else
           (error 'latexify "Unknown expression type: " expr)))))

