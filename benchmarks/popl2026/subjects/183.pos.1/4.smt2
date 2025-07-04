; Input: /benchmark/subjects/183.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.diff re.allchar _let_1) _let_1))))
(assert (>= (- (str.len s) 2) 0))
(assert (let ((_let_1 (str.len s))) (< (- _let_1 2) _let_1)))
(assert (not (distinct (str.at s (- (str.len s) 2)) "a")))
(check-sat)
(exit)