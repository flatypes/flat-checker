; Input: /benchmark/subjects/055.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.* re.allchar))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)