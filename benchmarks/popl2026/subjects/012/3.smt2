; Input: /benchmark/subjects/012.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s re.allchar))
(assert (>= (- (str.len s) 1) 0))
(assert (let ((_let_1 (str.len s))) (< (- _let_1 1) _let_1)))
(assert (not (str.in_re (str.at s (- (str.len s) 1)) re.allchar)))
(check-sat)
(exit)