; Input: /benchmark/subjects/011.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (>= 0 0))
(assert (< 0 (str.len s)))
(assert (not (str.in_re (str.at s 0) re.allchar)))
(check-sat)
(exit)