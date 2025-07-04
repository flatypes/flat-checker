; Input: /benchmark/subjects/240.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (str.in_re s (re.++ (re.++ (re.* _let_2) (re.union (re.diff re.allchar (re.union _let_2 _let_1)) (re.++ (re.++ _let_1 (re.* _let_1)) (re.diff re.allchar _let_1)))) (re.* re.allchar))))))
(assert (>= (str.indexof s "b" 0) 0))
(assert (not (and (<= 0 0) (<= 0 (str.indexof s "b" 0)))))
(check-sat)
(exit)