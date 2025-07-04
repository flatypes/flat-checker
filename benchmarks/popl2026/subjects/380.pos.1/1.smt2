; Input: /benchmark/subjects/380.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "b") (re.* (str.to_re "a")))))
(assert (distinct s "b"))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)