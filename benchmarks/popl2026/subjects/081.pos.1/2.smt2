; Input: /benchmark/subjects/081.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (>= 0 0))
(assert (< 0 (str.len s)))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)