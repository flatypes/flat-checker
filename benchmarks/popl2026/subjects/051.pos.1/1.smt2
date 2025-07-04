; Input: /benchmark/subjects/051.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar (re.* re.allchar))))
(assert (not (> (str.len s) 0)))
(check-sat)
(exit)