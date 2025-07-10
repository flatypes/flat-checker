; Input: /benchmark/subjects/145.py
(set-logic ALL)
(declare-const s String)
(assert (let ((_let_1 (re.* re.allchar))) (str.in_re s (re.++ (re.++ _let_1 (str.to_re "a")) _let_1))))
(assert (not (and (<= 0 0) (<= 0 (str.len s)))))
(check-sat)
(exit)