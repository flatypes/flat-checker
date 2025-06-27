; Input: /benchmark/subjects/350.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.++ (str.to_re "a") ((_ re.loop 0 1) re.allchar)) (str.to_re "b"))))
(assert (distinct s "ab"))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)