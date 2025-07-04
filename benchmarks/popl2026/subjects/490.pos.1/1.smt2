; Input: /benchmark/subjects/490.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "1"))) (let ((_let_2 (str.to_re "0"))) (str.in_re s (re.++ (re.++ (re.++ (re.* (re.union _let_2 _let_1)) _let_2) _let_1) _let_1)))))
(assert (not (and (>= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)