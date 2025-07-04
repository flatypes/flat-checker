; Input: /benchmark/subjects/381.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "b") (re.* (str.to_re "a")))))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)