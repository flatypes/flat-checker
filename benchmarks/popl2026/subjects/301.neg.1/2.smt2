; Input: /benchmark/subjects/301.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (str.to_re "b"))) (str.in_re s (re.union (re.++ _let_1 (re.* (re.++ (re.diff re.allchar _let_2) (re.* _let_2)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))
(assert (>= 0 0))
(assert (>= 2 0))
(assert (not (= (str.substr s 0 (- 2 0)) "ab")))
(check-sat)
(exit)