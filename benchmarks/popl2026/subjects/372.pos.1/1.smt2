; Input: /benchmark/subjects/372.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "a") (str.to_re "b"))))
(assert (distinct s "a"))
(assert (distinct s "b"))
(assert (not false))
(check-sat)
(exit)