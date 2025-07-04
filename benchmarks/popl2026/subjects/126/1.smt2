; Input: /benchmark/subjects/126.py
(set-logic ALL)
(declare-const s String)
(assert (str.in_re s (re.++ (str.to_re "a") (re.* re.allchar))))
(assert (let ((_let_1 (str.len s))) (not (and (<= _let_1 _let_1) (>= _let_1 0)))))
(check-sat)
(exit)