; Input: /benchmark/subjects/401.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (re.* (str.to_re "a")) (re.* (str.to_re "b")))))
(assert (not (and (<= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)