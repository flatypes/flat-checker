; Input: /benchmark/subjects/432.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ (re.++ _let_2 re.allchar) (re.* re.allchar)) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (not (and (>= 0 0) (< 0 (str.len s)))))
(check-sat)
(exit)