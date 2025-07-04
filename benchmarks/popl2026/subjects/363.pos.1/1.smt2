; Input: /benchmark/subjects/363.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.* re.allchar)) (str.to_re "b"))))
(assert (not (str.contains s "a")))
(check-sat)
(exit)