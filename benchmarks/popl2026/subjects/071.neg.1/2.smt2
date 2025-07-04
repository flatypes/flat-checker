; Input: /benchmark/subjects/071.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.opt re.allchar)))
(assert (>= 1 0))
(assert (>= (str.len s) 0))
(assert (not (str.in_re (str.substr s 1 (- (str.len s) 1)) (re.++ re.allchar (re.* re.allchar)))))
(check-sat)
(exit)