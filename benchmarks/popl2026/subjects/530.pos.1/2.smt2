; Input: /benchmark/subjects/530.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.* (re.union (re.diff re.allchar _let_1) (re.++ _let_1 (str.to_re "b")))))))
(assert (let ((_let_1 (str.to_re "a"))) (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.* (re.union (re.diff re.allchar _let_1) (re.++ _let_1 (str.to_re "b"))))))))
(check-sat)
(exit)