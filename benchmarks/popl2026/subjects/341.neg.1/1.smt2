; Input: /benchmark/subjects/341.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (let ((_let_3 (re.++ (re.diff re.allchar _let_2) (re.* re.allchar)))) (str.in_re s (re.union _let_3 (re.++ _let_2 (re.opt (re.++ (re.++ re.allchar (re.* (re.++ (re.++ _let_1 _let_2) re.allchar))) (re.union (re.++ _let_1 (re.union _let_2 _let_3)) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1)))))))))))))
(assert (not (and (<= 0 0) (< 0 (+ (str.len s) 3)))))
(check-sat)
(exit)