; Input: /benchmark/subjects/540.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "b"))) (let ((_let_2 (str.to_re "a"))) (let ((_let_3 (re.* (re.diff re.allchar _let_2)))) (let ((_let_4 (re.++ (re.++ _let_1 _let_3) _let_2))) (str.in_re s (re.++ (re.++ (re.++ _let_3 _let_2) (re.opt (re.++ (re.++ _let_1 (re.* (re.++ _let_4 _let_1))) (re.opt _let_4)))) (re.* (re.++ (re.diff re.allchar _let_1) (re.* _let_1))))))))))
(assert (not (and (<= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)