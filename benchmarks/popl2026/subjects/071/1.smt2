; Input: /benchmark/subjects/071.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) (re.* re.allchar))))
(assert (not (and (>= 1 0) (>= (str.len s) 0))))
(check-sat)
(exit)