; Input: /benchmark/subjects/352.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") (re.opt re.allchar)) (str.to_re "b"))))
(assert (= (str.len s) 2))
(assert (distinct (str.len s) 3))
(assert (not (= (str.at s 1) "b")))
(check-sat)
(exit)