; Input: /benchmark/subjects/066.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) re.allchar)))
(assert (<= (str.len s) 3))
(assert (not (and (>= 0 0) (>= 3 0))))
(check-sat)
(exit)