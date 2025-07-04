; Input: /benchmark/subjects/350.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.opt re.allchar)) (str.to_re "b"))))
(assert (distinct s "ab"))
(assert (not (= (str.len s) 3)))
(check-sat)
(exit)