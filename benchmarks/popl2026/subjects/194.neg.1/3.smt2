; Input: /benchmark/subjects/194.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.* (re.union (re.diff re.allchar _let_1) (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar)))))))
(assert (let ((_let_1 (- (str.len s) 1))) (not (and (<= _let_1 _let_1) (>= _let_1 0)))))
(check-sat)
(exit)