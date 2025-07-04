; Input: /benchmark/subjects/530.pos.1.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (str.to_re "a"))) (str.in_re s (re.* (re.union (re.diff re.allchar _let_1) (re.++ _let_1 (str.to_re "b")))))))
(assert (not (and (<= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)