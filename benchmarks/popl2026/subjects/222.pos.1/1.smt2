; Input: /benchmark/subjects/222.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.* (str.to_re "a")) (str.to_re "b"))))
(assert (not (str.contains s "b")))
(check-sat)
(exit)