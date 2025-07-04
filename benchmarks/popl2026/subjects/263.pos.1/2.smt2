; Input: /benchmark/subjects/263.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (re.opt (str.to_re "a")) (str.to_re "b"))))
(assert (not (= (str.at s (- (str.len s) 1)) "b")))
(check-sat)
(exit)