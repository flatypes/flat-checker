; Input: /benchmark/subjects/080.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "a")))
(assert (not (= s "a")))
(check-sat)
(exit)