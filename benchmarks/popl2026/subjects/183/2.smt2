; Input: /benchmark/subjects/183.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.diff re.allchar _let_1) _let_1))))
(assert (>= (- (str.len s) 1) 0))
(assert (let ((_let_1 (str.len s))) (< (- _let_1 1) _let_1)))
(assert (not (= (str.at s (- (str.len s) 1)) "a")))
(check-sat)
(exit)