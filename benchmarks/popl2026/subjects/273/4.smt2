; Input: /benchmark/subjects/273.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") ((_ re.loop 0 1) (str.to_re "b")))))
(assert (= (str.at s (- (str.len s) 1)) "b"))
(assert (not (= (str.at s 0) "a")))
(check-sat)
(exit)