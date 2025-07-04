; Input: /benchmark/subjects/350.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.* re.allchar))) (let ((_let_3 (str.to_re "b"))) (let ((_let_4 (re.++ _let_3 re.allchar))) (let ((_let_5 (re.diff re.allchar _let_3))) (str.in_re s (re.union (re.++ _let_1 (re.opt (re.union (re.++ _let_5 (re.union (re.++ _let_4 _let_2) (re.* (re.++ _let_5 (re.* _let_3))))) (re.++ (re.++ _let_3 (re.union _let_5 _let_4)) _let_2)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))))))
(assert (distinct s "ab"))
(assert (not (and (>= 2 0) (< 2 (str.len s)))))
(check-sat)
(exit)