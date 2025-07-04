; Input: /benchmark/subjects/053.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (distinct (str.len s) 1))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)