; Input: /benchmark/subjects/471.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "0"))) (str.in_re s (re.* (re.union _let_1 (re.++ _let_1 (str.to_re "1")))))))
(assert (distinct s ""))
(assert (not (and (<= 0 0) (<= 0 (- (str.len s) 1)))))
(check-sat)
(exit)