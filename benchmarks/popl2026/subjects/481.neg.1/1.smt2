; Input: /benchmark/subjects/481.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "1"))) (let ((_let_2 (str.to_re "0"))) (let ((_let_3 (re.diff re.allchar (re.union _let_2 _let_1)))) (let ((_let_4 (re.* _let_2))) (let ((_let_5 (re.++ _let_2 _let_4))) (str.in_re s (re.++ (re.++ _let_4 (re.union _let_3 (re.++ (re.++ _let_1 (re.* (re.++ _let_5 _let_1))) (re.union (re.diff re.allchar _let_2) (re.++ _let_5 _let_3))))) (re.* re.allchar)))))))))
(assert (distinct s ""))
(assert (not (and (<= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)