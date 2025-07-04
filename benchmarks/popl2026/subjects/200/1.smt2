; Input: /benchmark/subjects/200.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.++ (re.* _let_1) (re.diff re.allchar _let_1)))))
(assert (not (and (>= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)