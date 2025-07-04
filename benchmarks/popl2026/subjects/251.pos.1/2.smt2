; Input: /benchmark/subjects/251.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.* (re.++ (str.to_re "a") (str.to_re "b")))))
(assert (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.* (re.++ (str.to_re "a") (str.to_re "b"))))))
(check-sat)
(exit)