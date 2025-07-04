; Input: /benchmark/subjects/290.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.++ _let_1 re.allchar))) (let ((_let_3 (str.to_re "a"))) (str.in_re s (re.++ (re.union (re.union (re.diff re.allchar (re.union _let_3 _let_1)) (re.++ _let_3 (re.union (re.diff re.allchar _let_1) _let_2))) _let_2) (re.* re.allchar)))))))
(assert (not (or (or (or (= s "") (= s "a")) (= s "b")) (= s "ab"))))
(check-sat)
(exit)