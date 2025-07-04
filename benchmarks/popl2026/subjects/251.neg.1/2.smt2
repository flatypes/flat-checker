; Input: /benchmark/subjects/251.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.++ (re.diff re.allchar _let_1) (re.* re.allchar)))) (let ((_let_3 (str.to_re "b"))) (str.in_re s (re.union _let_2 (re.++ _let_1 (re.* (re.union (re.++ (re.diff re.allchar _let_3) (re.* _let_3)) (re.++ _let_3 (re.union _let_1 _let_2)))))))))))
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.++ (re.diff re.allchar _let_1) (re.* re.allchar)))) (let ((_let_3 (str.to_re "b"))) (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.union _let_2 (re.++ _let_1 (re.* (re.union (re.++ (re.diff re.allchar _let_3) (re.* _let_3)) (re.++ _let_3 (re.union _let_1 _let_2))))))))))))
(check-sat)
(exit)