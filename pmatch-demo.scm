;;;
;;; Name:     pmatch-demo.scm
;;; Purpose:  Demonstration of pattern matching.
;;; Author:   M. J. Fromberger <http://www.dartmouth.edu/~sting/>
;;; Info:     $Id$
;;;

(load "pmatch.scm")

;; Some rules for differentiation
(define *deriv-rules*
  (rules
   (on (dd ~c $v)         =>     0)  ;; d/dx c = 0
   (on (dd $v $v)         =>     1)  ;; d/dx x = 1
   (on (dd $u $v)         =>     0)  ;; d/dx y = 0 if y /= x
   (on (dd (?u + ?v) $x)  => 
       ((dd ?u $x) + (dd ?v $x)))    ;; d/dx (u + v) = d/dx u + d/dx v

   (on (dd (?u * ?v) $x)  =>
       ((?u * (dd ?v $x)) + 
	(?v * (dd ?u $x))))          ;; d/dx (u * v) = u d/dx v + v d/dx u

   (on (dd (?u ^ ~n) $x)  =>
       ((~n * (?u ^ (eval (- ~n 1)))) * 
	(dd ?u $x)))                 ;; d/dx (u ^ n) = n u^(n-1) d/dx u
   ))

;; Some rules for algebraic simplification
(define *algebra-rules*
  (rules
   (on (~c1 $op ~c2)      =>    (eval ($op ~c1 ~c2)))
   (on (- ~c)             =>    (eval (- ~c)))
   
   ;; Identity rules
   (on (0 + ?e)           =>    ?e)
   (on (1 * ?e)           =>    ?e)
   (on (0 * ?e)           =>    0)
   (on (?e ^ 1)           =>    ?e)
   (on (-1 * (- ?u))      =>    ?u)
   (on (-1 * ?u)          =>    (- ?u))
   (on (- (- ?u))         =>    ?u)
   
   ;; Move constants to the front, allowing identities to work
   (on (?e + ~c)          =>    (~c + ?e))
   (on (?e * ~c)          =>    (~c * ?e))
   
   ;; A general cancellation property
   (on (?e - ?e)          =>    0)
   
   ;; Simplifying equivalences, replacing an expr by a constant
   (on (?e + ?e)          =>    (2 * ?e))
   (on (?e * ?e)          =>    (?e ^ 2))
   
   ;; Shift associative operations leftward to group constants
   (on (~c1 * (~c2 * ?e)) =>    ((~c1 * ~c2) * ?e))
   (on (~c1 + (~c2 + ?e)) =>    ((~c1 + ~c2) + ?e))

   ;; Shift other associative operations rightward for following rules
   (on ((?u * ?v) * ?w)   =>    (?u * (?v * ?w)))
   (on ((?u + ?v) + ?w)   =>    (?u + (?v + ?w)))
   
   ;; Some tricks with commutativity/associativity
   (on (?u * (~c * ?v))   =>    (~c * (?u * ?v)))
   (on (?u + (~c + ?v))   =>    (~c + (?u + ?v)))
   (on (?u + (~c * ?u))   =>    ((eval (+ ~c 1)) * ?u))
   (on (?u - (~c * ?u))   =>    ((eval (- 1 ~c)) * ?u))
   (on ($x + (~c * $x))   =>    ((eval (+ ~c 1)) * $x))
   (on ((~c * $x) + $x)   =>    ((eval (+ ~c 1)) * $x))
   (on ((~c * $x) - $x)   =>    ((eval (- ~c 1)) * $x))
   (on ($x * ($x ^ ~c))   =>    ($x ^ (eval (+ ~c 1))))
   (on ((- ?u) + ?v)      =>    (?v + (- ?u)))
   (on (?u + (- ?v))      =>    (?u - ?v))
   
   ;; Some rules for exponentiation
   (on (($x ^ ~n) * 
        ($x ^ ~m))        =>    ($x ^ (eval (+ ~n ~m))))
   
   ;; Distributive law
   (on ((~c * ?e) +
        (~d * ?e))        =>    ((~c + ~d) * ?e))
   (on ((~c * ?e) -
        (~d * ?e))        =>    ((~c - ~d) * ?e))
   (on (~c * (?u + ?v))   =>    ((~c * ?u) + (~c * ?v)))
   (on (~c * (?u - ?v))   =>    ((~c * ?u) - (~c * ?v)))
   (on ($x * (?u + $x))   =>    ((?u * $x) + ($x ^ 2)))
   ))

(define *macro-rules*
  (rules
   ;; Turning "let*" into nested let
   (on (let* () . ?body) => (let () . ?body))
   (on (let* (?bind . ?binds) . ?body) =>
       (let (?bind)
	 (let* ?binds . ?body)))
   
   ;; Turning "named let" into "letrec"
   (on (let $name ?binds . ?body) => (nlaccum () () $name ?body . ?binds))
   (on (nlaccum ?vars ?exprs $name ?body . ()) =>
       (letrec (($name (lambda ?vars . ?body)))
	 ($name . ?exprs)))
   (on (nlaccum ?vars ?exprs $name ?body ($v ?e) . ?binds) =>
       (nlaccum ($v . ?vars) (?e . ?exprs) $name ?body . ?binds))
   
   ;; Turning "cond" into "if" and "begin"
   (on (cond (else . ?body)) => (begin . ?body))
   
   (on (cond (?test . ?body) . ?claws) =>
       (if ?test (begin . ?body) (cond . ?claws)))
   
   ;; Collapse "begin" forms with only one expression
   (on (begin ?only) => ?only)

   ;; The standard examples, "unless" and "when"
   (on (when ?test . ?body)   => (if ?test (begin . ?body)))
   (on (unless ?test . ?body) => (if (not ?test) (begin . ?body)))

   ;; A kind of variant conditional with binding
   (on (if $name := ?expr ?conseq) =>
       (let (($name ?expr))
	 (if $name ?conseq)))

   (on (if $name := ?expr ?conseq ?alter) =>
       (let (($name ?expr))
	 (if $name ?conseq ?alter)))
   ))

(define simplify (pdriver *algebra-rules*))
(define deriv    (pdriver *deriv-rules*))
(define sderiv   (compose simplify deriv))
(define mexpand  (pdriver *macro-rules*))

;; Unlimit the length and depth of printing (DrScheme)
;(*print-length* #f)
;(*print-level*  #f)

;; Turn on debugging output
(set! *debug-level* 1)

;; Some simple tests to try
;(simplify '(0 + (3 * x)))
;(simplify '((3 * (x ^ 3)) + ((2 * x) * (x ^ 2))))
;(simplify '((3 * x) + (((x * 2) - x) * 3)))
;(deriv    '(dd (+ (* x 2) 3) x))
;(deriv    '(dd ((3 * (x ^ 2)) + (2 * x)) x))
;(sderiv   '(dd (+ (* x 2) 3) x))
;(sderiv   '(dd ((3 * (x ^ 2)) + (2 * x)) x))
;(sderiv   '(dd ((x * 3) + x) x))
