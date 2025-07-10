; Input: /benchmark/subjects/471.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "0"))) (str.in_re s (re.* (re.union _let_1 (re.++ _let_1 (str.to_re "1")))))))
(assert (distinct s ""))
(assert (let ((_let_1 (str.to_re "1"))) (let ((_let_2 (str.to_re "0"))) (not (str.in_re (str.substr s 0 (- (str.len s) 0)) (re.++ (re.opt _let_1) (re.* (re.union _let_2 (re.++ _let_2 _let_1)))))))))
(check-sat)
(exit)