; Input: /benchmark/subjects/180.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ _let_2 (re.union (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar)) (re.* (re.++ _let_2 (re.* _let_1))))) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (>= 1 0))
(assert (< 1 (str.len s)))
(assert (not (= (str.at s 1) "a")))
(check-sat)
(exit)