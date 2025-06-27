; Input: /benchmark/subjects/271.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") ((_ re.loop 0 1) (str.to_re "b")))))
(assert (= (str.len s) 1))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)