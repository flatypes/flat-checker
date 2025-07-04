; Input: /benchmark/subjects/272.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ (re.++ _let_1 (re.union (re.diff re.allchar _let_2) (re.++ _let_2 re.allchar))) (re.* re.allchar)) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (distinct (str.indexof s "b" 0) 1))
(assert (not (= s "a")))
(check-sat)
(exit)