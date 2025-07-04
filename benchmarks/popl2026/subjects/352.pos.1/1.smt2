; Input: /benchmark/subjects/352.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.opt re.allchar)) (str.to_re "b"))))
(assert (= (str.len s) 2))
(assert (not (= (str.len s) 2)))
(check-sat)
(exit)