; Input: /benchmark/subjects/051.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (not (> (str.len s) 0)))
(check-sat)
(exit)