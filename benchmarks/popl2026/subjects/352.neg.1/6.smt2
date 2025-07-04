; Input: /benchmark/subjects/352.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.* re.allchar))) (let ((_let_3 (str.to_re "b"))) (let ((_let_4 (re.++ _let_3 re.allchar))) (let ((_let_5 (re.diff re.allchar _let_3))) (str.in_re s (re.union (re.++ _let_1 (re.opt (re.union (re.++ _let_5 (re.union (re.++ _let_4 _let_2) (re.* (re.++ _let_5 (re.* _let_3))))) (re.++ (re.++ _let_3 (re.union _let_5 _let_4)) _let_2)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))))))
(assert (= (str.len s) 2))
(assert (= (str.len s) 3))
(assert (not (and (>= 1 0) (< 1 (str.len s)))))
(check-sat)
(exit)