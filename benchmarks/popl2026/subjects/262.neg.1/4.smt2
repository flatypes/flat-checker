; Input: /benchmark/subjects/262.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (let ((_let_3 (re.union _let_2 _let_1))) (let ((_let_4 (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar)))) (str.in_re s (re.union (re.union (re.++ _let_2 (re.union _let_4 (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))) _let_4) (re.* (re.++ (re.diff re.allchar _let_3) (re.* _let_3))))))))))
(assert (= (str.indexof s "b" 0) 1))
(assert (not (= (str.len s) 2)))
(check-sat)
(exit)