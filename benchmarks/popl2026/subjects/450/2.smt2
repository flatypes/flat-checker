; Input: /benchmark/subjects/450.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (str.in_re s (re.* (re.++ (re.++ _let_2 (re.diff re.allchar (re.union _let_2 _let_1))) _let_1))))))
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.* (re.++ (re.++ _let_2 (re.diff re.allchar (re.union _let_2 _let_1))) _let_1)))))))
(check-sat)
(exit)