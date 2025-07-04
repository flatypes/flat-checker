; Input: /benchmark/subjects/002.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (distinct s ""))
(assert (not false))
(check-sat)
(exit)