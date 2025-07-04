; Input: /benchmark/subjects/232.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* (str.to_re "b")))))
(assert (not (str.contains s "a")))
(check-sat)
(exit)