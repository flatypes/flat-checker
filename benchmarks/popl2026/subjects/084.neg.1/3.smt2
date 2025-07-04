; Input: /benchmark/subjects/084.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.union (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar)) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))
(assert (not (= (str.len s) 1)))
(check-sat)
(exit)