; Input: /benchmark/subjects/183.neg.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (let ((_let_2 (re.diff re.allchar _let_1))) (str.in_re s (re.union (re.++ _let_2 (re.union (re.++ (re.++ _let_1 re.allchar) (re.* re.allchar)) (re.* (re.++ _let_2 (re.* _let_1))))) (re.* (re.++ _let_1 (re.* _let_2))))))))
(assert (>= (- (str.len s) 2) 0))
(assert (let ((_let_1 (str.len s))) (< (- _let_1 2) _let_1)))
(assert (not (distinct (str.at s (- (str.len s) 2)) "a")))
(check-sat)
(exit)