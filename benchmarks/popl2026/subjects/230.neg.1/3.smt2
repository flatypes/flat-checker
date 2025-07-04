; Input: /benchmark/subjects/230.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ (re.++ (re.++ _let_1 (re.* _let_2)) (re.diff re.allchar _let_2)) (re.* re.allchar)) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (not (and (>= 1 1) (<= 1 (str.len s)))))
(check-sat)
(exit)