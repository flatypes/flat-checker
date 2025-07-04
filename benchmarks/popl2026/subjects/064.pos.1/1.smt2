; Input: /benchmark/subjects/064.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ re.allchar re.allchar) re.allchar)))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)