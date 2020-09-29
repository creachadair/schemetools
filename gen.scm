;;; 
;;; Name:     gen.scm
;;; Purpose:  Python-style generators for Scheme.
;;; Author:   M. J. Fromberger <http://www.dartmouth.edu/~sting/>
;;; Info:     $Id: gen.scm 490 2006-09-12 20:06:45Z sting $
;;; 
;;; Notes:
;;; Adds a new special form (implemented as a hygienic macro) which behaves
;;; as follows:
;;;     (generator value (params ...) body...)
;;; Gives as result a "generator procedure", in the body of which, occurrences
;;; of the form (yield expr) causes execution of the procedure body to be sus-
;;; pended, and the value of expr returned.
;;;
;;; When this procedure is called, it returns a thunk which, when it is called,
;;; evaluates the body of the generator until a "yield" is invoked, in which
;;; case the yielded value is returned, or until evaluation of the body is 
;;; complete, in which case the "value" given to the generator is returned.
;;;
;;; Bugs:  Does not support nested generators (yet).
;;;
;;; This implementation was based on examples of how to capture names inside
;;; hygienic macros described by Al Petrofsky in comp.lang.scheme.
;;;
(define-syntax capture-name
  (syntax-rules ()
    ((_ name replacement form)
     (let-syntax ((name replacement))
       form))))

;; (find-id id form (s . sargs) fail)
;; ==> (s id . args) if id is found;
;; ==> fail otherwise
;; This technique borrowed from Al Petrofsky (thanks!)
;;
(define-syntax find-id
  (syntax-rules ()
    ((_ id (head . rest) succ fail)
     (find-id id head succ (find-id id rest succ fail)))
    ((_ id #(stuff ...) succ fail)
     (find-id id (stuff ...) succ fail))
    ((_ id form succ fail)
     (let-syntax ((scan (syntax-rules (id)
                          ((_ id id* (s* . sarg*) fail*)
                           (s* id* . sarg*))
                          ((_ other form succ* fail*)
                           fail*))))
       (scan form form succ fail)))
    ))

(define-syntax let/cc
  (syntax-rules ()
    ((_ name body ...)
     (call/cc (lambda (name)
                body ...)))))

(define-syntax generator
  (syntax-rules ()
    ((_ stop (arg ...) body1 body2 ...)
     (lambda (arg ...)
       (letrec ((resume (find-id
                         yield (body1 body2 ...)
                         (capture-name (syntax-rules ()
                                         ((_ value)
                                          (let/cc cont
                                            (set! resume cont)
                                            (set! resume-val value)
                                            (return resume-val))))
                                       (lambda (v)
                                         body1 body2 ...
                                         (let/cc cont
                                           (set! resume cont)
                                           (set! resume-val v)
                                           v)))
                         (lambda (v)
                           body1 body2 ... 
                           (let/cc cont
                             (set! resume cont)
                             (set! resume-val v)
                             v))))
                (resume-val stop)
                (return #f))
         (lambda ()
           (let/cc rtn
             (set! return rtn)
             (resume resume-val)))
         )))))
