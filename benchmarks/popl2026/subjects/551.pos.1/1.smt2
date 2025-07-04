; Input: /benchmark/subjects/551.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.diff re.allchar _let_1))) (let ((_let_3 (re.* _let_2))) (str.in_re s (re.union (re.++ (re.union (re.++ (re.++ _let_2 _let_3) _let_1) (re.++ _let_1 re.allchar)) (re.* re.allchar)) _let_3))))))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)