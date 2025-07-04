; Input: /benchmark/subjects/053.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (str.to_re "")))
(assert (= (str.len s) 1))
(assert (not (str.in_re "" (re.union (str.to_re "") re.allchar))))
(check-sat)
(exit)