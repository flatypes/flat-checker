; Input: /benchmark/subjects/371.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.union (str.to_re "a") (str.to_re "b"))))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)