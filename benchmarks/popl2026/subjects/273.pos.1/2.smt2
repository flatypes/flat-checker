; Input: /benchmark/subjects/273.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.opt (str.to_re "b")))))
(assert (= (str.at s (- (str.len s) 1)) "b"))
(assert (not (= (str.len s) 2)))
(check-sat)
(exit)