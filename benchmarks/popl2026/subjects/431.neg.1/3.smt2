; Input: /benchmark/subjects/431.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ (re.++ _let_2 re.allchar) (re.* re.allchar)) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (distinct (str.at s 0) "a"))
(assert (distinct (str.at s 0) "c"))
(assert (not (distinct (str.at s 0) "b")))
(check-sat)
(exit)