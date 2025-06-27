; Input: /benchmark/subjects/045.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (not (and (>= 1 0) (< 1 (str.len s)))))
(check-sat)
(exit)