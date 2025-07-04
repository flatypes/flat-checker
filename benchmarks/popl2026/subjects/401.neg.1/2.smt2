; Input: /benchmark/subjects/401.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (str.in_re s (re.++ (re.union (re.union (re.diff re.allchar (re.union _let_2 _let_1)) (re.++ (re.++ _let_2 (re.* _let_2)) (re.diff re.allchar _let_2))) (re.++ (re.++ _let_1 (re.* _let_1)) (re.diff re.allchar _let_1))) (re.* re.allchar))))))
(assert (let ((_let_1 (= 0 0))) (not (ite (str.contains s "a") _let_1 _let_1))))
(check-sat)
(exit)