;;;
;;; Name:     pmatch.scm
;;; Purpose:  General-purpose pattern matching for list structures.
;;; Author:   M. J. Fromberger <http://www.dartmouth.edu/~sting/>
;;; Info:     $Id: pmatch.scm,v 1.4 2006/09/12 20:01:34 sting Exp $
;;;

;; --- Pattern matching
;;
;; An expression is any Scheme primitive or list structure.  Two
;; expressions are equal if all their component parts are equal.
;;
;; A pattern is an expression, except that patterns may contain
;; special objects called "pattern variables" that denote spaces or
;; "holes" in the pattern to be filled in later.
;;
;; An expression is said to "match" a pattern if you can find values
;; to substitute for each of the pattern variables in the pattern,
;; such that the result is equal to the expression.
;;
;; A matching function takes a pattern and an expression, and if the
;; expression matches the pattern, returns dictionary in which each
;; key is a pattern variable and each value is the object you would
;; have to substitute for that variable to make the pattern equal the
;; expression.
;;
;; To construct a matching function, we need to be able to identify
;; what objects are pattern variables, and for any given pattern
;; variable, which objects match it.  We can do this using two
;; functions:
;; 
;; (is-pvar? obj)
;;   ==> true if obj is a pattern variable, otherwise #f.
;; (vmatch? v e)
;;   ==> true if expression e matches pattern variable v, else #f.

;; By making the "vmatch?" function a parameter, we can customize what
;; it means for an expression to "match" a variable -- for instance,
;; we can have different kinds of pattern variables, each of which
;; matches a different type of expression.

;; The make-matcher function returns a matching procedure
;; (pm pattern expression)

(define make-matcher
  (lambda (is-pvar? vmatch?)
    (lambda (pat expr)
      (letrec ((pm
                (lambda (pat expr dict)
                  (cond ((not dict) dict)
                        
                        ((is-pvar? pat)
			 (and (vmatch? pat expr)
			      (let ((found (assq pat dict)))
				(if found
				    (and (equal? (cdr found) expr)
					 dict)
				    (cons (cons pat expr) dict)))))
                        
                        ((pair? pat)
                         (and (pair? expr)
                              (pm (cdr pat) (cdr expr)
                                  (pm (car pat) (car expr)
                                      dict))))
                        
                        ((pair? expr) #f)
                        
                        (else
                         (and (equal? pat expr) dict))))))
        (pm pat expr '())))))

;; --- Template instantiation
;;
;; A pattern can be thought of as a kind of "expression template," in
;; which the pattern variables mark the locations of expression parts
;; that are not yet known.  If you know what values you want to fill
;; in for the pattern variables, you can "instantiate" the template,
;; by replacing all the pattern variables with their values.

;; A template instantion function takes a template and a dictionary of
;; pattern variables, and constructs a new expression by replacing
;; each pattern variable in the template with its value from the
;; dictionary.  To construct a template instantiation function, we
;; need a function to identify pattern variables:
;;
;; (is-pvar? obj)
;;   ==> true if obj is a pattern variable, otherwise #f.

;; In addition to replacing pattern variables, it's sometimes nice to
;; be able to handle certain kinds of expressions in some "special"
;; way while instantiating.  We can parameterize this behaviour with
;; two additional functions:
;;
;; (is-special? expr)
;;   ==> true if expr should be handled "specially", otherwise #f.
;;
;; (specfill filler expr dict)
;;   Given a pattern instantation function (filler), an expression
;;   that has been identified as special (expr), and a dictionary of
;;   pattern variables (dict), return a new expression to replace
;;   expr.
;; 
;; The make-filler function returns template instantion procedure
;; (pf expr dict).
;;
(define make-filler
  (lambda (is-pvar? is-special? specfill)
    (letrec ((pf
              (lambda (expr dict)
                (cond ((is-pvar? expr) 
		       (let ((found (assq expr dict)))
			 (if found (cdr found) expr)))
                      
                      ((is-special? expr)
                       (specfill pf expr dict))
                      
                      ((pair? expr)
                       (cons (pf (car expr) dict)
                             (pf (cdr expr) dict)))
                      
                      (else expr)))))
      pf)))

;; --- Replacement driver
;;
;; Pattern matching and template instantiation can be used to build a
;; powerful model of computation based on "rewrite rules".  A "rule"
;; is just a pattern combined with a template, and a rule r can be
;; "applied" to an expression e using the following logic:
;; 
;; If e matches pattern(r) with dictionary d, then
;;   replace e with instantiate(tempate(r), d).
;;
;; This idea can be generalized to an ordered sequence R of rules, 
;; 
;; to apply R to e:
;;   for r in R:
;;     if e matches pattern(r) with dictionary d, then
;;       apply R to instantiate(template(r), d)
;; 
;; In other words, we find the first rule that matches, rewrite the
;; expression according to that rule, and then look for another
;; matching rule, until you get to an expression that does not match
;; any rules.
;;
;; To automate this process, we will build a "driver" function that
;; takes an expression and applies a set of rules to it.  We need
;; three things to do this:
;;
;; 1. A matching function (as described above).
;; 2. A template instantiation function (as described above).
;; 3. A list of rules.
;;
;; For this implementation, a "rule" will be represented as a pair,
;; whose head is the pattern and whose tail is the template; the rules
;; will be presented as an ordinary Scheme list.
;;
;; The make-driver function takes (1) and (2) as input, and returns a
;; procedure taking (3) as input.  That procedure in turn returns the
;; actual "driver" function, which maps input expressions to output
;; expressions by applying the rewrite rules.
;;
(define make-driver
  (lambda (matcher filler)
    (lambda (rules)
      (letrec ((rewrite
                (lambda (expr)
                  (let next ((cur (if (list? expr)
                                      (map rewrite expr)
                                      expr))
                             (more rules))
                    (cond ((null? more)
                           cur)
                          ((matcher (caar more) cur) =>
                           (lambda (dict)
			     (let ((result 
				    (filler (cdar more) dict)))
			       (dprintf 1
				"~a matches ~a~n with   ~a~n giving ~a~n~n"
				cur (caar more) 
				(format-alist dict) 
				result)
			       (rewrite result))))
                          (else
                           (next cur (cdr more))))))))
        rewrite))))

;; --- Some standard definitions

;; A pattern variable is any symbol that begins with
;; either ? or ~ or $ as its first character.
(define is-pvar?
  (lambda (obj)
    (and (symbol? obj)
         (memq (first-glyph obj) '(#\? #\~ #\$)))))

;; A pattern variable ?x matches any object; a pattern variable ~x
;; matches any number; a pattern variable $x matches any symbol.
(define vmatch?
  (lambda (sym expr)
    (case (first-glyph sym)
      ((#\~) (number? expr))
      ((#\$) (symbol? expr))
      (else #t))))

;; An expression is "special" if it is a list of the form (eval EXPR).
(define math-special?
  (lambda (expr)
    (and (list? expr)
         (= (length expr) 2)
         (eq? (car expr) 'eval))))

;; To handle (eval EXPR), instantiate the EXPR and then interpret the
;; results according to a couple of simple Scheme-like rules.
(define math-specfill
  (lambda (pf expr dict)
    (let ((filled (pf (cadr expr) dict)))
      (if (or (null? filled) (not (list? filled)))
          filled
          (case (car filled)
            ((+) (apply + (cdr filled)))
            ((-) (apply - (cdr filled)))
            ((*) (apply * (cdr filled)))
            ((/) (apply / (cdr filled)))
	    ((^) (apply expt (cdr filled)))
            ((mod) (apply modulo (cdr filled)))
            (else filled))))))

;; --- Utility functions

;; Return the first character of a symbol
(define first-glyph
  (lambda (sym)
    (string-ref (symbol->string sym) 0)))

;; A macro to simplify the expression of rules
;; (rules
;;   (on PATTERN => TEMPLATE) ...)
;; expands to a list of (PATTERN . TEMPLATE) pairs.  You do not need
;; to quote the pattern or the template.
;;
(define-syntax rules
  (syntax-rules (rule on =>)
    ((rules) '())
    ((rules (on pat => expr) more ...)
     (cons (cons (quote pat) (quote expr)) 
	   (rules more ...)))))

;; Basic unary function composition
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; String join
(define join
  (lambda (joiner strs)
    (cond ((null? strs) "")
	  ((null? (cdr strs)) (car strs))
	  (else
	   (string-append (car strs) joiner
			  (join joiner (cdr strs)))))))

;; Format an alist for human consumption
(define format-alist
  (lambda (lst)
    (join ", " (map (lambda (pr)
		      (format "~a = ~a"
			      (car pr) (cdr pr)))
		    lst))))

;; Format a list of rules for human consumption
(define format-rules
  (lambda (lst)
    (string-append
     "(rules\n  "
     (join "\n  " (map (lambda (rule)
                         (format "(on ~a => ~a)"
                                 (car rule) (cdr rule)))
                       lst))
     ")")))

;; Debugging output level
(define *debug-level* #f)

;; This function behaves like DrScheme's printf, but it only generates
;; output if the global *debug-level* is set to a value greater than
;; or equal to the "level" argument.
;;
(define dprintf
  (lambda (level fmt . args)
    (if (and *debug-level* (>= *debug-level* level))
	(apply printf fmt args))))

;; --- Example usage ------------------------------------------------------

;; Define matcher, instantiator, and driver for algebraic rules
(define pmatch  (make-matcher is-pvar? vmatch?))
(define pfill   (make-filler is-pvar? math-special? math-specfill))
(define pdriver (make-driver pmatch pfill))

;; Here there be dragons