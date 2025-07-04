; Input: /benchmark/subjects/222.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (let ((_let_3 (re.union _let_2 _let_1))) (str.in_re s (re.* (re.union (re.union _let_2 (re.++ (re.diff re.allchar _let_3) (re.* _let_3))) (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar)))))))))
(assert (not (= (str.indexof s "b" 0) (- (str.len s) 1))))
(check-sat)
(exit)