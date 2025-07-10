; Input: /benchmark/subjects/032.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* re.allchar)))
(assert (distinct (str.len s) 0))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)