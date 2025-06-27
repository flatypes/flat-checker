; Input: /benchmark/subjects/093.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s ((_ re.loop 0 1) (str.to_re "a"))))
(assert (distinct (str.len s) 0))
(assert (distinct s "a"))
(assert (not false))
(check-sat)
(exit)