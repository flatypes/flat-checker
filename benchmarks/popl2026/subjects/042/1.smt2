; Input: /benchmark/subjects/042.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ re.allchar re.allchar)))
(assert (> (str.len s) 2))
(assert (not false))
(check-sat)
(exit)