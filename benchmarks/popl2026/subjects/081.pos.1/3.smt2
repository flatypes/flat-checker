; Input: /benchmark/subjects/081.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)