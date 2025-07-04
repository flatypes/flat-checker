; Input: /benchmark/subjects/233.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ (re.++ (re.++ _let_1 (re.* _let_2)) (re.diff re.allchar _let_2)) (re.* re.allchar)) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (let ((_let_1 (+ 0 1))) (not (and (<= 1 _let_1) (<= _let_1 (str.len s))))))
(check-sat)
(exit)