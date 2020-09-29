;; (collision-probability outcomes choices)
;; Compute the probability that at least two of the given number
;; of choices come out to the same outcome, upon selecting at 
;; random from a number of equiprobable outcomes.
;; 
;; For example, the probability of rolling the same number on
;; two 6-sided dice is (collision-probability 6 2)
;; 
(define collision-probability
  (lambda ((outcomes <integer>)
           (choices <integer>))
 
    (let loop ((out 1.0)
               (left (- choices 1))
               (mult (- outcomes 1)))
      
      (cond ((zero? left) (- 1. out))
            (else
             (loop (* out (/ mult outcomes))
                   (- left 1)
                   (- mult 1)))))))

(define find-choices
  (lambda ((outcomes <integer>)
           (threshold <real>))
    
    (let loop ((low 1) 
               (p-low (collision-probability outcomes 1))
               (high outcomes)
               (p-high (collision-probability outcomes outcomes)))
      
        (if (and (>= (+ low 1) high)
                 (<= p-low threshold)
                 (>= p-high threshold))
            high
            (let* ((mid   (quotient (+ low high) 2))
                   (p-mid (collision-probability outcomes mid)))
              (cond ((> p-mid threshold)
                     (loop low p-low mid p-mid))
                    
                    ((< p-mid threshold)
                     (loop mid p-mid high p-high))

                    (else 
                     (loop mid p-mid mid p-mid))))
        ))))
              