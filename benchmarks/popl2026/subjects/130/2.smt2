; Input: /benchmark/subjects/130.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ ((_ re.^ 0) re.allchar) (re.* re.allchar)) (str.to_re "a"))))
(assert (not (= (str.at s (- (str.len s) 1)) "a")))
(check-sat)
(exit)