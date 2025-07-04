; Input: /benchmark/subjects/020.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar))))
(assert (not (<= (str.len s) 1)))
(check-sat)
(exit)