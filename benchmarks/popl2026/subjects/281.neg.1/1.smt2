; Input: /benchmark/subjects/281.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (re.* re.allchar))) (let ((_let_3 (str.to_re "a"))) (str.in_re s (re.union (re.++ (re.diff re.allchar _let_3) _let_2) (re.++ _let_3 (re.union (re.++ (re.++ _let_1 re.allchar) _let_2) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))))))
(assert (> (str.len s) 0))
(assert (not (= s "ab")))
(check-sat)
(exit)