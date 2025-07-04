; Input: /benchmark/subjects/990_lsb_check.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "1"))) (let ((_let_2 (str.to_re "0"))) (let ((_let_3 (re.union _let_2 _let_1))) (str.in_re s (re.* (re.union (re.union _let_2 (re.++ (re.diff re.allchar _let_3) (re.* _let_3))) (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar)))))))))
(assert (not (and (>= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)