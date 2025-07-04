; Input: /benchmark/subjects/510.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "c"))) (let ((_let_2 (re.++ _let_1 re.allchar))) (let ((_let_3 (str.to_re "b"))) (let ((_let_4 (re.++ _let_3 (re.union (re.diff re.allchar _let_1) _let_2)))) (let ((_let_5 (str.to_re "a"))) (str.in_re s (re.++ (re.union (re.union (re.union (re.diff re.allchar (re.union (re.union _let_5 _let_3) _let_1)) (re.++ _let_5 (re.union (re.union (re.diff re.allchar (re.union _let_3 _let_1)) _let_4) _let_2))) _let_4) _let_2) (re.* re.allchar)))))))))
(assert (not (or (or (or (or (or (or (or (= s "abc") (= s "ab")) (= s "a")) (= s "ac")) (= s "bc")) (= s "b")) (= s "c")) (= s ""))))
(check-sat)
(exit)