; Input: /benchmark/subjects/085.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (distinct (str.len s) 1))
(assert (not false))
(check-sat)
(exit)