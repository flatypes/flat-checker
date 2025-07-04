; Input: /benchmark/subjects/201.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.* (re.union _let_1 (re.++ (re.++ (re.diff re.allchar _let_1) re.allchar) (re.* re.allchar)))))))
(assert (>= (- (str.len s) 1) 0))
(assert (let ((_let_1 (str.len s))) (< (- _let_1 1) _let_1)))
(assert (not (distinct (str.at s (- (str.len s) 1)) "a")))
(check-sat)
(exit)