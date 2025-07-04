; Input: /benchmark/subjects/530.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.* (re.diff re.allchar _let_1)))) (let ((_let_3 (str.to_re "b"))) (str.in_re s (re.++ (re.++ _let_2 _let_1) (re.* (re.union (re.++ (re.diff re.allchar _let_3) (re.* _let_3)) (re.++ (re.++ _let_3 _let_2) _let_1)))))))))
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.* (re.diff re.allchar _let_1)))) (let ((_let_3 (str.to_re "b"))) (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.++ (re.++ _let_2 _let_1) (re.* (re.union (re.++ (re.diff re.allchar _let_3) (re.* _let_3)) (re.++ (re.++ _let_3 _let_2) _let_1))))))))))
(check-sat)
(exit)