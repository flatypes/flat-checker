; Input: /benchmark/subjects/065.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) re.allchar)))
(assert (not (= (- (str.len s) 1) 2)))
(check-sat)
(exit)