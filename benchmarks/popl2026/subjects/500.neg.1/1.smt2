; Input: /benchmark/subjects/500.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "c"))) (let ((_let_2 (re.++ (re.++ _let_1 (re.* _let_1)) (re.diff re.allchar _let_1)))) (let ((_let_3 (str.to_re "b"))) (let ((_let_4 (str.to_re "a"))) (str.in_re s (re.++ (re.++ (re.* _let_4) (re.union (re.union (re.diff re.allchar (re.union (re.union _let_4 _let_3) _let_1)) (re.++ (re.++ _let_3 (re.* _let_3)) (re.union (re.diff re.allchar (re.union _let_3 _let_1)) _let_2))) _let_2)) (re.* re.allchar))))))))
(assert (not (and (<= 0 0) (<= 0 (ite (str.contains s "b") (str.indexof s "b" 0) (ite (str.contains s "c") (str.indexof s "c" 0) (str.len s)))))))
(check-sat)
(exit)