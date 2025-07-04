; Input: /benchmark/subjects/054.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (>= 0 0))
(assert (>= (str.len s) 0))
(assert (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (str.to_re ""))))
(check-sat)
(exit)