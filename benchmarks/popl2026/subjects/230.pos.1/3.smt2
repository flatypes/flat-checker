; Input: /benchmark/subjects/230.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* (str.to_re "b")))))
(assert (not (and (>= 1 1) (<= 1 (str.len s)))))
(check-sat)
(exit)