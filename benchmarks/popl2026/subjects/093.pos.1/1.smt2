; Input: /benchmark/subjects/093.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt (str.to_re "a"))))
(assert (distinct (str.len s) 0))
(assert (distinct s "a"))
(assert (not false))
(check-sat)
(exit)