; Input: /benchmark/subjects/391.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.union (str.to_re "a") (str.to_re "b")))) (str.in_re s (re.++ (re.union (re.diff re.allchar _let_1) (re.++ _let_1 re.allchar)) (re.* re.allchar)))))
(assert (distinct (str.len s) 0))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)