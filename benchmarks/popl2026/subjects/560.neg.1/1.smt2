; Input: /benchmark/subjects/560.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (let ((_let_3 (re.union _let_2 _let_1))) (str.in_re s (re.union (re.++ _let_2 (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))) (re.* (re.++ (re.diff re.allchar _let_3) (re.* _let_3)))))))))
(assert (not (str.contains s "b")))
(check-sat)
(exit)