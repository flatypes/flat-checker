; Input: /benchmark/subjects/273.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.opt (str.to_re "b")))))
(assert (distinct (str.at s (- (str.len s) 1)) "b"))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)