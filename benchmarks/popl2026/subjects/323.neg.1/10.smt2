; Input: /benchmark/subjects/323.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.opt (re.++ re.allchar (re.union (re.++ (re.++ _let_2 re.allchar) (re.* re.allchar)) (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2))))))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (not (= (str.at (str.substr s 1 (- (str.len s) 1)) 1) "b")))
(check-sat)
(exit)